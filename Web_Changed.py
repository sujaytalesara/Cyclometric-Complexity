# -*- coding: utf-8 -*-
"""
Created on Sat Dec  2 23:31:51 2017

@author: sujay
"""

import web

# Modified the web.py run method to accept different ports that are passed
class MyWebApp(web.application):
    def run(self,port=8080,*middleware):
        func = self.wsgifunc(*middleware)
        return web.httpserver.runsimple(func, ('0.0.0.0',port))