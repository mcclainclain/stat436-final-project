#!/usr/bin/env python
# coding: utf-8

# In[1]:


# Selenium Imports
from selenium import webdriver
from selenium.webdriver.chrome.service import Service
from selenium.webdriver.common.keys import Keys
from selenium.webdriver.common.by import By
from selenium.webdriver.support.ui import Select, WebDriverWait
from selenium.webdriver.support import expected_conditions as EC
from selenium.webdriver.common.action_chains import ActionChains

import pandas as pd
import time

from io import StringIO


# In[2]:


# Set up the driver
service = Service(executable_path="C:/Users/matth/geckodriver-v0.35.0-win32/geckodriver.exe")
options = webdriver.FirefoxOptions()
options.binary_location = r'C:\Program Files\Mozilla Firefox\firefox.exe'
# options.add_argument('-headless')
driver = webdriver.Firefox(service=service, options=options)


# In[3]:


driver.get("https://www.wra.org/Resources/Property/Wisconsin_Housing_Statistics/")
time.sleep(5)

# Get Statewide Data
all_wi = pd.read_html(StringIO(driver.page_source))

# Get County Data
region_data = {}
county_data = {}
regions_select = Select(driver.find_element(By.XPATH, "//*[@id=\"region-list\"]"))
options = regions_select.options

for i in range(1, len(options)):
    
    regions_select.select_by_index(i)
    time.sleep(5)
    regions_select = Select(driver.find_element(By.XPATH, "//*[@id=\"region-list\"]"))
    region_data[regions_select.first_selected_option.text] = pd.read_html(StringIO(driver.page_source))
    
    county_select = Select(driver.find_element(By.XPATH, "//*[@id=\"county-list\"]"))
    county_options = county_select.options
    for j in range(1, len(county_options) - 1):
        
        county_select.select_by_index(j)
        time.sleep(5)
        county_select = Select(driver.find_element(By.XPATH, "//*[@id=\"county-list\"]"))
        county_data[county_select.first_selected_option.text] = pd.read_html(StringIO(driver.page_source))
        
    regions_select = Select(driver.find_element(By.XPATH, "//*[@id=\"region-list\"]"))
        
driver.quit()
    


# In[4]:


# Create a copy of the data
region_data_copy = region_data.copy()
county_data_copy = county_data.copy()
all_wi_copy = all_wi.copy()


# In[5]:


for key, value in region_data_copy.items():
    region_data_copy[key] = {
        "Homes Sold": value[1],
        "Median Sales Price": value[2],
    }


# In[6]:


for key, value in county_data_copy.items():
    county_data_copy[key] = {
        "Homes Sold": value[1],
        "Median Sales Price": value[2],
    }


# In[7]:


all_wi_copy = {
    "Homes Sold": all_wi_copy[1],
    "Median Sales Price": all_wi_copy[2],
}


# In[11]:


counties_homes_sold = pd.DataFrame()
for key, value in county_data_copy.items():
    df = value["Homes Sold"]
    df["County"] = key
    df = df.rename({"Unnamed: 0": "Year"}, axis=1).drop(columns=["YTD"])
    counties_homes_sold = pd.concat([counties_homes_sold, df])


# In[18]:


counties_median_price = pd.DataFrame()
for key, value in county_data_copy.items():
    df = value["Median Sales Price"]
    df["County"] = key
    df = df.rename({"Unnamed: 0": "Year"}, axis=1).drop(columns=["YTD"])
    counties_median_price = pd.concat([counties_median_price, df])


# In[21]:


counties_median_price.to_csv("wi_median_prices.csv")
counties_homes_sold.to_csv("wi_homes_sold.csv")


# In[ ]:




