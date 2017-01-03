from flask import Flask, request, render_template, Response
from flask_cors import CORS, cross_origin
import json

from time import time, sleep

from os.path import getmtime as modTime

import os
from subprocess import Popen
from threading import Thread

import code

codeThread = None
tempBuffer = None
pin, pout = os.pipe()

app = Flask(__name__)
CORS(app)

def runCode():
	system("runhaskell buffer.hs")

@app.route("/process", methods=['POST'])
def process():
	return "got code, now generating results!"

@app.route("/editor", methods=['GET', 'POST'])
def retrieve():
	if request.method=='GET':
		f = open("buffer.hs", "r")
		retval = f.read()
		f.close()
		return render_template("index.html", code=retval)
	else:
		print "Recieved:", request.form 
		retval = "Success!"
		execStr = ""
		print request.form
		code = request.form['code']
		mode = "w"

		if "{**haskAppend=true**}" in code:
			mode = "a"
			code = code.replace("{**haskAppend=true**}", "\n")

		f = open("buffer.hs", mode)
		f.write(code)
		f.close()

		#codeThread = Thread(target=runCode)
		#codeThread.start()

		return retval

@app.route("/results", methods=['GET', 'POST'])
def showResults():
	if request.headers.get('accept') == 'text/event-stream':
		def events():
			#try:
				updateTime = time()
				startTime = time()
				while True:
					fileTime = modTime("./results.io")
					if abs(updateTime - fileTime)>0.01:
						updateTime = fileTime
						print "Found update!"
						f = open("results.io", "r")
						yield "event: normaldata: "+f.read().replace('\n', '<br>')+"\n\n"
						f.close()
					#elif time()-startTime>1:
					#	print "KILL NOW"
					#	raise KeyboardInterrupt
					else:
						print time()-startTime
						yield "event: heartbeat\n\n"
			#except:
			#	print "EXCEPTION"
		return Response(events(), content_type='text/event-stream')
	return "THIS IS NOT WHAT YOU WERE LOOKING FOR"

if __name__ == "__main__":
	#t = Thread(target=writer)
	#t.start()
	app.run()