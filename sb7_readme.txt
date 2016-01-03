This is the README for StormBot.TCL version 7.0 by Mai 'Domino' Mizuno (Domino@stormbot.org).

Here is how you you should set-up SB7 with your eggdrop:

Step #0:
     # unzip / untar the script in the bot's HOME directory (this is a change!): 
     cd <bot's home directory>
     tar -zxvf <stormbot tarball>
     # All files will dump into "scripts/sb7/"

Step #1:
     cd scripts/sb7
     edit "sb7.tcl" updating your preferred values. 
     stop where it says "STOP EDITING HERE"
     re-save as "sb7.tcl"

Step #2:
     add this line to your eggdrop bot at the BOTTOM (make it the last line)
     (Do not SOURCE any other script fragments other than this!)
     source scripts/sb7/sb7.tcl

Step #3:
     if your bot is alive, REHASH it:
          if this is the first time you've put a script on your bot,
          you can REHASH one of three ways:
               * ".REHASH" from DCC CHAT
               * "/msg bot rehash <password>"
               * "kill -SIGHUP <PID>" from your shell

     if it's not active, start it now (bot's directory: "./eggdrop [-m] <config file>")

Step #4:
     if your bot was running StormBot.TCL Version 3, 4, 5, or 6:

          * you will have to edit sb7.tcl from scratch. None of the values
               from previous versions carry over (I did this on purpose).

          * remove the old "source scripts/sb#.tcl" from your config file.

          * RESTART instead of REHASH your bot.

          * you do NOT need to keep any older version running.

          * the bot will try to update / purge old data internally.

Step #5:
     get some popcorn, soda, runts, and enjoy your bot!

====================================================================================

StormBot.TCL version 7.0 is released under the BEERWARE license:

[based on: http://people.freebsd.org/~phk/]

----------------------------------------------------------------------------
"THE BEER-WARE LICENSE" (Revision 43):

Dave Hansen (xone@stormbot.org) and Mai Mizuno (domino@stormbot.org)
wrote this script, with contributions by: Justin Hammond, Dave Klein, 
& Dustin Lennon. As long as you retain this notice you can do whatever you 
want with this stuff. If we meet some day, and you think this stuff is 
worth it, you can buy everyone a beer (and Mai a vanilla creme soda or 
strawberry soda) in return.
----------------------------------------------------------------------------

Just kidding. I'm releasing this under GPL V3. Study it. Learn from it. Use 
it. Make beads. So long as proper credit of who really wrote this script 
exists, I am sufficiently happy.

If you want to make / donate a module, see me, and I'll help you.
Once the script is finalized, I will accept donations and include donated
module beads. I =ALWAYS= give credit to donated code / beads / contributions.

As always, help can be found in-vivo:

IRC.BOTHOUSE.NET in #BotHouse
[ irc://irc.bothouse.net:6667/Bothouse ]
[ ircs://irc.bothouse.net:7001/Bothouse ]

Enjoy!

~ Mai Mizuno (domino@stormbot.org)

# StormBot.TCL version 7.X series, add-on script for EGGDROP V1.3 - V1.8
# (C) 2012 - 2016: [V7.0      ] David P. Hansen, Mai Mizuno
# (C) 2009 - 2016: [V6.0 - 6.1] David P. Hansen, Mai Mizuno
# (C) 2003 - 2010: [V5.0 - 5.2] David P. Hansen, Mai Mizuno
# (C) 2000 - 2003: [V4.0 - 4.3] David P. Hansen, Mai Mizuno
# (C) 2002 - 2003: [V3.5      ] David P. Hansen, Mai Mizuno
# (C) 2000 - 2003: [V3.1b- 3.2] David P. Hansen, Mai Mizuno
# (C) 1998 - 2000: [V3.0 - 3.1] David P. Hansen, Dave Klein, Justin Hammond, & Mai "Domino" Mizuno
# (C) 1997 - 1998: [V1.0 - 2.0] David P. "Xone" Hansen, Dave "CyberMac" Klein, & Justin "ElriC" Hammond

