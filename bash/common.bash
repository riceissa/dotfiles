# This file is for bash configuration that I would like to use on every
# machine.

# Modified from <https://www.jefftk.com/p/you-should-be-logging-shell-history>
promptFunc() {
    # right before prompting for the next command, save the previous
    # command in a file.
    echo "$(date -Iseconds) $(hostname) $PWD $(history 1)" \
        >> ~/.full_history
}
PROMPT_COMMAND=promptFunc

# Pressing CTRL-D is easiest way to exit the shell, but it does not leave a
# line in the bash history.  Since the promptfunc history logging above logs
# the most recently run command, this means that whatever "real" command one
# ran last will get logged again and again every time one opens a new shell
# window. Try this: with the promptfunc above enabled, type "less +F
# ~/.full_history" to watch the file, then open some new shell windows. You
# will see that whatever terminal command you happened to last run (from a
# shell session that has already ended) will keep getting logged. To prevent
# this sort of garbage logging, one solution is to standardize on the final
# command one runs before exiting the shell; this way, anytime one opens a new
# shell window, the standard command will be logged. And because it's a
# standard command, it will not be confusing to look at the full history: the
# standard command just means a new shell window was opened. One natural choice
# for the standard final command is "exit"; we want to just close the
# commandline after all. However, typing "exit" each time to close the
# commandline is a little annoying. The following alias makes exiting a little
# less cumbersome. It also never exits when there are jobs, which is quite
# useful on its own.
alias e='if [[ $(jobs) ]]; then jobs; else exit; fi'
