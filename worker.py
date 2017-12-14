# -*- coding: utf-8 -*-
"""
Created on Sat Dec  2 23:25:50 2017

@author: sujay
"""

import web
import os
import Web_Changed
from git import Repo
import sys
import lizard
import requests as req
import threading



class worker(threading.Thread):
    def run(self):
        urls = (
        '/worker', 'worker'
        )
        app = Web_Changed.MyWebApp(urls,globals())
        app.run(port=port)
    def GET(self):
        return 0



    def POST(self):
        return 0


if __name__=="__main__":
    if len(sys.argv) != 3:
        print("Please provide host and port number")
        exit()
    host = sys.argv[1]
    port = int(sys.argv[2])
    first_time = 1


   