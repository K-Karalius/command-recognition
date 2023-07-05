# command-recognition
Recognition of the command "POP reg" and "POP sreg"

The application searches for "POP reg" and "POP sreg" commands between 61st and 74th line (between "nop" and "popf" commands):
![commands](/readMeImages/info3.png)

You can modify these lines with differnt commands.

# Running the application

### Requirements
Download [DOSBOX](https://www.dosbox.com/download.php?main=1) \
We are also going to need TASM, which i have added to this repository.

### How to run
1. Put .asm file and 2 .txt files with binary numbers into the TASM folder.
2. Open DOSBOX application
3. mount a virtual drive, for example, named `a` by typing the following `mount a *TASM folder location*`
4. type `a:` (name of the drive)
5. Assemble the .asm file into the object file by typing `tasm fileName.asm` \
 ![Now you will see the list of erros, warnings and etc.](/readMeImages/info1.png)
6. To get the .exe file type `tlink fileName.obj` (should be the same as .asm) \
 ![tlink](/readMeImages/info2.png)
7. Now you can launch the application by typing `fileName.exe`
8. Now you should see the results, like in the example below : \
![commands](/readMeImages/info3.png) ![output](/readMeImages/info4.png)
