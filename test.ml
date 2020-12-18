(**
   INCLUDE: 
   - auto vs. manual
   - which module
   - black box, glass box, dev method
   - why testing demonstrates correctness

   Done: Admin, Client, Survey, Main, Command
   TODO: State

   Admin Module: 
    Development Method: We used test driven development and developed tests
    using black box testing. Since the functions written were not complex and
    did not have any branches (i.e.: if statements or pattern matching), we 
    created test cases based on the specifications of each functions and
    possible edge cases.

    OUnit Testing: make_admin, get_id, get_name, get_login, update_pword, 
    to_json, read_json, encrypt

    Manual: none 

    Correctness: The testing suite demonstrates correctness of the admin module
    because all exposed functions were tested based on their specifications, or
    how what they are expected to output given that the precondition is not 
    violated.  

   Client Module: 
    Development Method: We used test driven development, and developed tests
    using black box and glass box testing. We utilized black box testing for
    simple functions like make_user, get_id, get_name, get_login, get_prefs,
    get_matches, get_logins, update_pword, encrypt, update_prefs, 
    update_matches, update_notifs, and incr_logins. We used black and glass box 
    testing for the user_of_uid function because it utilizes pattern matching 
    and an if statement, so we ensured that all possible branches were covered.

    OUnit Testing: make_user, get_id, get_name, get_login, get_prefs,
    get_matches, get_logins, update_pword, encrypt, update_prefs, 
    update_matches, update_notifs, user_of_uid, incr_logins

    Manual: none

    Correctness: The testing suite demonstrates correctness of the client module
    because all exposed functions were tested based on their specifications, or
    how what they are expected to output given that the precondition is not 
    violated. In addiiton, we tested all possible branches for the function that
    contained pattern matching and an if statement. 

   Survey Module:
    Development Method: We used test driven development, and developed tests 
    using both black box and glass box testing, depending on complexity of the 
    function being tested. For relatively simple functions that just read from
    a json file (question_list, answer_list, compile_matches, match_score) or 
    did simpler manipulations of records (type_of_question, check_ans, etc.) 
    we used black box testing. These functions did not contain any mutability, 
    so black-box testing was sufficient. To test the creation of our question 
    histogram we used glass-box testing. Because question_histogram 
    returns unit and prints out the created histogram, we used glass-box
    testing to ensure the histogram data was being created properly. The
    test_hist_values function accomplishes this by callig on a helper fucnction
    used by question_histogram.

    OUnit Testing: question_list, answer_list, check_ans, type_of_question, 
    match_score, compile_matches, test_hist_values, exception testing for 
    functions that raise failures 

    Manual: The question_histogram function was tested manually, because it 
    prints the created histogram to the terminal. This is additional to the 
    OUnit tests done on question_histogram's helper functions and
    question_histogram's raise Failure scenario. The manual testing was 
    primarily to ensure formatting was clear and as intended.
    We also tested exception handling manually, ensuring that inputs not 
    following specifications were handled correctly. We also tested the
    print_question function manually because it prints to the terminal and
    returns a unit. 

    Correctness: The testing suite for Survey demonstrates correctness because
    all exposed functions were tested on a set of json files with known output,
    and tests pass as expected. Survey primarily interacts with the survey json
    file.

   Main Module:
    Development Method: We tested Main manually, because this module performs
    input/output interactions with the terminal.

    Manual Testing: Our manual tests included running through each sign up and
    log in method for both Admins and Users, using various system states. 
    After creating new users and admins, we then logged back in to 
    those accounts to confirm that input for username, password, and survey 
    answers were all saved correctly. Main also includes functions to take in
    and respond to user commands. These functions were tested manually and 
    automatically (see testing for Command Module). For manual testing, we 
    called each command from various accounts, ensuring that the output matched
    with account settings and the currect system state. For example, the
    send_notif command was tested by first sending a notification from user1 to
    user2, then logging into user2 account to ensure the message was received 
    and viewable. Other commands were tested similarly, according to their 
    expected behavior as outlined in the documentation.

    Correctness: This style of testing demonstrates correctness because we 
    are able to evaluate how input is handled by inputting commands that build
    on each other and evaluating that the output to terminal is according to 
    expected behavior.

   Command Module:
    Development Method: We used test driven development and black box testing 
    to ensure that command parsing followed its specifications. We used black
    box testing so that we could test each of the if/else statements in both
    parse_user and parse_admin.

    OUnit Testing: parse_user, parse_admin

    Correctness: For both parse_user and parse_admin, we ran tests for proper 
    output and to test that the correct exceptions were raised. Because we used
    black box testing, we tested each branch of the if/else statements for a 
    comprehensive test suite based on the specifications.

    State Module: 
    Development Method: We used test driven development and developed tests 
    using black box, glass box, and manual testing. We created test cases based 
    on the specifications of each functionsand possible edge cases to ensure 
    most pattern matches and if branches were tested.

    OUnit Testing: get_users, add_users, get_user_by_id, validate_user, 
    replace_user, user_can_sign_up, get_user_recs (in test_survey.ml), 
    get_admins, add_admins, get_admin_by_id, validate_admin, admin_can_sign_up.

    State.get_state and init_state were implicitly tested while testing the 
    above functions

    Manual: draw_graph and shortest_path were tested manually. The graphs and 
    shortest paths produced were black-box tested for several edge cases, 
    eg. empty graph, one graph, user(s) not in graph, and shortest_path to self.
    The functions print_user_stats, read_notifs, and print_matches were tested
    manually because they each return unit and print strings to the console.
    The functions store_users, store_admins, and consequently, 
    send_notifications; change_user_password; and change_admin_password were  
    tested manually because they manipulate the Users.json and Admins.json file,
    and therefore cannot be tested by the OUnit Suite. 

    Correctness: The testing suite demonstrates correctness of the State module
    because all exposed functions were tested based on their specifications, or
    how what they are expected to output given that the precondition is not 
    violated.  

*)