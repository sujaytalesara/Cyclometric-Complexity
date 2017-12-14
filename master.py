# -*- coding: utf-8 -*-
"""
Created on Sat Dec  2 23:22:48 2017

@author: sujay
"""

import web
import os
import Web_Changed
from git import Repo
import requests as req
import threading

urls = (
'/master', "master",
'/register/', "register",
'/result', "done_work"
)

class master:
    def GET(self):
        return "hello"
    def POST(self):
        # get the passed parameters host and port from the url
        worker_details = web.input(host='',port='')
        web.config.lock.acquire()
        if web.config.pointer <= len(web.config.commit_files):
            # get the git commit hex and filename from commit_files dictionary
            #print(web.config.pointer)
            (commithex,filename) = web.config.commit_files[web.config.pointer]
            #print((commithex,filename))
            web.config.pointer = web.config.pointer+1
            web.config.lock.release()
            # call the worker webservice for doing work by passing commit hex and filename
            url = "http://"+worker_details.host+":"+worker_details.port+"/worker?commithex="+commithex+"&filename="+filename
            print(url)
            print("pointer: ",web.config.pointer)
            print("length: ",len(web.config.commit_files))
            response = req.get(url)
            return "Done"


        else:
            web.config.lock.release()
            return "No task to assign!"




class register:
    def GET(self):
        # To register the worker who is active.
        web.config.worker_num = web.config.worker_num+1
        return "Active"

class done_work:
    def POST(self):
       
        return "Work done!"


# count of no:of workers
global worker_num
worker_num = 0
# iterator through global dictionary commit_files that is used to assign work to each worker
pointer = 1
# dictionary that stores id(integers) as key and (commt hex,filename) as value
commit_files={}
# count the results recieved from each worker after completing their work
counter = 0
# for adding the cyclomatic complexity result from worker_result
result_sum = 0

if __name__=="__main__":

    app = Web_Changed.MyWebApp(urls,globals())
    web.config.update({"worker_num":0,"pointer":1,"counter":0,"result_sum":0,"commit_files":{},"lock":threading.Lock()})
    # get the local git repo
    repo = Repo("E:/GitHub/Distributed-File-Server")
    # get the list of commits
    commit_list = list(repo.iter_commits('master'))
    # get the files in each commit
    i=0
    for each_commit in commit_list:
        #commit_files[each_commit.hexsha]=(list(each_commit.stats.files.keys()))
        # Create a dictionary with id as key and (commit hex,filename) as value
        for filename in (list(each_commit.stats.files.keys())):
            if os.path.splitext(filename)[1] not in [".txt",".csv",".pdf",".md","",".pyc"]:
                web.config.commit_files[i+1] = (each_commit.hexsha,filename)
                i=i+1
    print(web.config.commit_files)
    print(len(web.config.commit_files))


    app.run(port=8080)