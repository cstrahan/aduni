#####! /usr/bin/python

###from ns_python import *
import cgi
#import string

reshtml = '''Content-type: text/html\n
<html><head><title>Exercise 3 answered</title></head>
<body bgcolor=white text=black>
<p>
Distance to Subject: %s feet (%s inches)<br>
Height of Subject: %s feet (%s inches)<br>
Magnification Needed: %s<br>
The focal length of the lens: %s inches<br>
The focal length in millimeters: %s mm<br>
<hr>
<address>
<a href="mailto:tom_hickerson@hotmail.com">tom_hickerson@hotmail.com</a>
</body>
</html>'''

errhtml = '''Content-type: text/html\n
<html><head><title>Exercise 3 answered</title></head>
<body bgcolor=white text=black>
<p><h3>__error__</h3>
<b>%s</b>
<form><input type=button value=Back To Form
onclick="window.history.back()"></form>
</body>
</html>'''

def showError(error_str):
    print errhtml % (error_str)

def process():
    error = ''
    form = cgi.FieldStorage()
    if form.has_key('dist_feet'):
       dist_subj_feet = float(form['dist_feet'].value)
    else: 
       error = 'Please enter the distance in feet<br>'
    if form.has_key('subj_feet') and (float(form['subj_feet'].value) > 0):
       subj_height_feet = float(form['subj_feet'].value)
    elif form.has_key('subj-feet'):
       error = error + 'Please enter a value greater than zero for subject height<br>'
    else:
       error = error + 'Please enter the height of the object<br>'
    if not error:
       dist_subj_inch = dist_subj_feet * 12
       subj_height_inch = subj_height_feet * 12
       mag = (1.5/subj_height_inch)              
       lens_focal_length = (dist_subj_inch/((1/mag) + 1))
       lens_mm = (round(lens_focal_length * 25.4))         
       print reshtml % (dist_subj_feet,dist_subj_inch,subj_height_feet,subj_height_inch,mag,lens_focal_length,lens_mm)
    else:
       showError(error)


if __name__ == '__main__':
    process()




