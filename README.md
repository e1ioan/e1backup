# e1backup

Windows service for daily backups to Amazon's S3

To compile the source, you'll need Delphi XE2 or later (I used XE5)

To install the service, create a directory somewhere on your windows computer, for example:

D:\e1backup

Copy the .exe that you find in the e1backup.zip to D:\e1backup\e1backup.exe

Create a ini directory: D:\e1backup\ini and copy the params.ini into it:
D:\e1backup\ini\params.ini

Edit D:\e1backup\ini\params.ini and fill in your AWS Key (Acount Name), Secret (Acount Key) and the bucket (you'll have to create the bucket first manually).
Set the backup time you want, for example 7:30 in the morning would be 
7:30 AM

or 5:31 in the afternoon would be

5:31 PM

List all the folders you want to backup in format:

Folder1=c:\temp

Folder2=d:\test

Folder3=e:\my-games\doc

Folder4=c:\some-other\

Folder5=e:\more-of-the-same\

Install the service
----------------------

Start command prompt as administrator and then go to the directory where you have the e1backup.exe:

d:
cd e1backup

and run the following command:

e1backup.exe /install

Go into your services and start the e1backup service.


Note
------

If in the params.ini you change the Enabled to 0 from 1 in the Amazon section, the backups will be made and just kept locally (you won't need an S3 account).

That's all.
