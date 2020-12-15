(** 
   The user interface for the system. 

   When the system starts, the user will be prompted to create a profile 
   (admin or non-admin) or sign in. 

   If the user opts to create a non-admin profile, the system will record their
   preferences by storing the user's answers to a series of questions through 
   the command line. After answering the survey, the user will see their list of 
   matches. This user will then enter a "waiting room" where he or she can enter
   various commands to send messages to matches, reset passwords, etc. 

   If the user opts to create an admin profile, they will enter a username and 
   password. Once the username and password are validated, the admin will be
   taken to the waitroom, where they can enter admin-related commands.

   If the user opts to log in, their creditials will be validated and they will
   have a chance to check their notifications before enter the aforementioned
   "waiting room".

   The user will enter a chat room and speak with a match until the command 
   "quit" is typed. The system will exit on the "quit" command. 
*)