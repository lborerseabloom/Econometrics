import os
import shutil
import time
from selenium import webdriver
from selenium.webdriver.common.by import By
from selenium.webdriver.chrome.service import Service
from webdriver_manager.chrome import ChromeDriverManager
from selenium.webdriver.support.ui import WebDriverWait
from selenium.webdriver.support import expected_conditions as EC
from selenium.common.exceptions import StaleElementReferenceException, TimeoutException

def Scraper(file_name, example_file):
    # --- Setup Chrome with custom download folder ---
    download_dir = os.path.abspath("downloads")
    os.makedirs(download_dir, exist_ok=True)

    options = webdriver.ChromeOptions()
    prefs = {"download.default_directory": download_dir}
    options.add_experimental_option("prefs", prefs)

    driver = webdriver.Chrome(service=Service(ChromeDriverManager().install()), options=options)
    driver.get("https://cde.ucr.cjis.gov/LATEST/webapp/#/pages/explorer/crime/crime-trend")

    wait = WebDriverWait(driver, 20)
    input("Press Enter to continue...") # Keeping the initial long sleep as it might be necessary for the page to fully load

    # --- Open the dropdown to get the list of ORIs ---
    try:
        input_box = wait.until(EC.element_to_be_clickable((By.ID, "agency-select-input")))
        input_box.click()

        # --- Get a stable list of ORIs upfront ---
        options_list = wait.until(EC.presence_of_all_elements_located((By.TAG_NAME, "nb-option")))
        ori_list = []
        for option in options_list:
            try:
                text_lines = option.text.split("\n")
                ori_line = [line for line in text_lines if "ORI:" in line][0]
                ori = ori_line.split("ORI:")[1].strip()
                ori_list.append(ori)
            except StaleElementReferenceException:
                # If the list becomes stale while iterating, refetch it
                options_list = wait.until(EC.presence_of_all_elements_located((By.TAG_NAME, "nb-option")))
                continue

        # --- Close the dropdown before starting the loop ---
        input_box.click()

    except TimeoutException as e:
        print(f"Failed to load the initial agency list: {e}")
        driver.quit()
        exit()


    # --- Loop over each ORI ---
    for ori in ori_list:
        print(f"--- Processing ORI: {ori} ---")
        try:
            # --- Re-open dropdown in each iteration ---
            input_box = wait.until(EC.element_to_be_clickable((By.ID, "agency-select-input")))
            input_box.click()

            # --- Use a specific XPath to find the option and click it ---
            option_xpath = f"//nb-option[contains(., 'ORI: {ori}')]"
            option_to_click = wait.until(EC.element_to_be_clickable((By.XPATH, option_xpath)))
            selected_option_text = option_to_click.text.replace('\n', ' ')
            print(f"Selecting: {selected_option_text}") 
            option_to_click.click()

            wait.until(EC.invisibility_of_element_located((By.CSS_SELECTOR, "nb-spinner")))

            # --- Click hamburger menu ---
            menu_icon = wait.until(
                EC.element_to_be_clickable((By.XPATH, "//nb-icon[@id='hr-menu-icon']/ancestor::button"))
            )
            menu_icon.click()

            # --- Click "Download as CSV" ---
            download_item = wait.until(
                EC.element_to_be_clickable((By.XPATH, "//li[@title='Download as CSV']"))
            )
            download_item.click()

            # --- Wait for download to complete and rename ---
            timeout = 1.5
            wait_time = 0
            download_complete = False

            while wait_time < timeout:
                # Look for the file in the download directory
                for f in os.listdir(download_dir):
                    if f.startswith(example_file):
                        latest_file = os.path.join(download_dir, f)
                        new_name = os.path.join(download_dir, f"{ori}_{file_name}.csv")
                        shutil.move(latest_file, new_name)
                        print(f"Downloaded and renamed to: {new_name}")
                        download_complete = True
                        break
                time.sleep(.5)
                wait_time += .5

            if not download_complete:
                print(f"Download timed out for ORI {ori}. Skipping.")
                continue

            # --- Reset dropdown for next iteration ---
            clear_button = wait.until(EC.element_to_be_clickable((By.XPATH, "//button[contains(@title,'Clear Agency')]")))
            clear_button.click()
            time.sleep(1)

        except Exception as e:
            print(f"An error occurred for ORI {ori}: {e}. Refreshing page and continuing.")
            # Refresh the page to reset the state in case of an error
            driver.refresh()
            time.sleep(5) # Wait for page to reload
            continue

    driver.quit()

if __name__=="__main__":
    file_name = input("Input file type:" )
    example_file = input("Input file example:" )
    Scraper(file_name, example_file)