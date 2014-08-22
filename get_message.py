# -*- coding: utf-8 -*-
"""
Created on Tue Aug 12 10:17:55 2014

@author: richardc
"""
import os
import email
from argparse import ArgumentParser

def grab_attachment(msg):
    filename = "None"
    attachment = "None"
    for part in msg.walk():
        if part.get_content_maintype() == 'multipart':  # this is just a placeholder
            continue
        filename = part.get_filename()
        if not filename:
            continue
        if not filename.endswith("xlsx"):
            filename = "Not xlsx"
            continue
        attachment = part.get_payload(decode=True)
    return(filename, attachment)


fetch_uid = '1'

# result, data = mail.uid('fetch', fetch_uid, '(RFC822)')

# check for OK

raw_email = data[0][1]
e = email.message_from_bytes(raw_email) # works !

mail_status = result
mail_from = e['From']
mail_date = e['Date']
mail_subject = e['Subject']
mail_attachment_name, mail_attachment = grab_attachment(e)

