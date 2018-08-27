# -*- coding: utf-8 -*-
"""
Created on Wed Dec 27 17:27:24 2017

@author: ah0667765
"""
    
import pyautogui
for i in range(10000):
    pyautogui.moveTo(100,200,duration = 0.25)
    pyautogui.moveTo(200,100,duration = 0.25)
    pyautogui.moveTo(200,300,duration = 0.25)
    pyautogui.moveTo(300,100,duration = 0.25)
    pyautogui.click(100,500)
    print(pyautogui.position)
                                
                                 
                                 