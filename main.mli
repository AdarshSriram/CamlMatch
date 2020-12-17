(** 
   The user interface for the system. 

   When the system starts, the user will be prompted to create a profile 
   (admin or non-admin) or sign in. 

   If the user opts to create a non-admin profile, the system will record their
   preferences by storing the user's answers to a series of questions through 
   the command line. After answering the survey, the user will see their list of 
   matches. This user will then enter a "waiting room" where he or she can enter
   various commands to send messages to matches, reset passwords, etc. 
   Further information about commands can be found in documentation for the 
   Command Module.

   If the user opts to create an admin profile, they will enter a username and 
   password. Once the username and password are validated, the admin will be
   taken to the wait room, where they can enter admin-related commands to view
   user information. Further information about commands can be found in 
   documentation for the Command Module.

   If the user opts to log in, their credentials will be validated and they will
   have a chance to check their notifications before entering the aforementioned
   "waiting room".They may then enter user commands as described above.

   If an admin opts to log in, their credentials will be validated and they will
   enter the aforementioned "waiting room". They may then enter admin commands
   as described above.

   For both users and admins, the system will exit on the "quit" command. 
*)