import selenium
from selenium import webdriver
from selenium.webdriver.common.by import By
from selenium.webdriver.support.ui import Select
from selenium.common.exceptions import NoSuchElementException
from selenium.webdriver.common.keys import Keys
import time
import 

geckodriver =  'D:\Users\\byrne\Downloads\geckodriver-v0.18.0-win64\geckodriver.exe'
driver = webdriver.Firefox()

wb = openpyxl.load_workbook('D:/College/Proposal 2/Crash data/TSTNO_with_CG.xlsx')
ws = wb.get_sheet_by_name('Sheet1')
for row in ws.iter_rows('A{}:A{}'.format(ws.min_row,ws.max_row)):
    for cell in row:
        print cell.value
        linknumber = str(cell.value)
        pagenumber = linknumber.zfill(5)

        driver.get('https://www-nrd.nhtsa.dot.gov/database/VSR/veh/QueryTest.aspx')
        time.sleep(2)
        
        inputnumber = driver.find_element_by_id("txtTstNoFrom")
        inputnumber.send_keys(pagenumber)
        inputnumber.send_keys(Keys.ENTER)
        time.sleep( 5 ) # need to wait a couple of seconds before the page opens so i can click on the opened webpage

        inputclick = driver.find_element_by_link_text(linknumber)
        inputclick.click()
        time.sleep( 5 )
        
        filetype = driver.find_element_by_xpath("//select[@name='lstTestFormat']/option[text()='NHTSA EV5 ASCII X-Y']")
        filetype.click()
        download = driver.find_element_by_id("cmdGo")
        download.click()
        time.sleep( 10 )
