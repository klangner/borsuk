# 
# Main file for running Borsuk server
#
from flask import Flask


app = Flask(__name__)


@app.route("/")
def hello():
    return "Hello World!"
