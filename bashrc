# ~/.bashrc: executed by bash(1) for non-login shells.
# see /usr/share/doc/bash/examples/startup-files (in the package bash-doc)
# for examples

#Resetting vim to be the pager
#The PAGER variable is the program with which man is displayed.
export PAGER="/bin/sh -c \"unset PAGER; col -b -x | \
   vim -R -c 'set ft=man nomod nolist' -c 'map q : q<CR>' \
   -c 'map <SPACE> <C-D>' -c 'map b <C-U>' \
   -c 'nmap K :Man <C-R>=expand(\\\"<cword>\\\")<CR><CR>' -\""
export PYTHONSTARTUP=/home/alex/.pythonrc
export EXT_LLVM_DIR=/home/alex/Downloads/llvm-3.2.src/build

#the following allows shell buffer editting in vi style. (In case you haven't
#noticed, I like vim).
set -o vi

#While referring to my bash history I often try to understand what I did and
#and why I did it. These aliases to 'sudo' convey not only what I was doing,
#but also how I felt doing it. 'please apt-get install' is for the first attempt
#fucking apt-get install' is for when I'm frustrated doing sys-admin work.
alias please=sudo
alias fucking=sudo

alias py=python

shopt -s cdspell
                                    #HISTORY
#If I put these two in scripts, I get history expansion (not of the parent 
#shell's history, but of the script's history)
set -o history 
set -o histexpand 
# append to the history file, don't overwrite it
shopt -s histappend
#Makes the size of the history unlimited
export HISTFILESIZE=
export HISTSIZE=

# don't put duplicate lines in the history. See bash(1) for more options
# don't overwrite GNU Midnight Commander's setting of `ignorespace'.
HISTCONTROL=$HISTCONTROL${HISTCONTROL+:}ignoredups
# ... or force ignoredups and ignorespace
HISTCONTROL=ignoreboth

                              #CONVENIENCE MACROS
alias kj="kill -9 `jobs -p`"

function work {
    cd /home/alex/Programming/compBio
}

function cdl {
    cd $1
    ls
}

function screen? {
  if [ $TERM == "screen" ]
  then
      echo "yes"
  else
      echo "no"
  fi
}
#Often, in the course programming, I will go through a 'text-edit' -> 'compile'
# -> 'run' -> 'text-edit' ... cycle. Rather than hit the up arrow key thrice
#before every command, one can simply use this 'cyc' command. Since the use case
#above is common, 3 is the default cycle length, but that can be overriden by 
#defining an environment variable named CYCLE to the desired integer value.
function cyc { 
    if [ -z $CYCLE ]; then 
       CYCLE=3
    fi
    eval $( fc -nl -$CYCLE -$CYCLE )
}

#Computes the relative frequency of the two commands given as command line 
#arguments
function freqCmp {
    #in the output of the 'history' command, a bash command comes in the 
    #second column, so I need to compare the function arguments to $2. There
    #is a namespace issue where both bash and awk have their own ideas of what
    #$2 means, so the variable $awk2 is a workaround to do things bash's way.
    awk2='$2'
    fstCnt=`history | awk "{if (\"$1\" == $awk2) print }" | wc -l`
    sndCnt=`history | awk "{if (\"$2\" == $awk2) print }" | wc -l`
    echo | awk "{print $fstCnt / $sndCnt}"
}

#Syntax for iterating over command line arguments
 #for i in "$@"
 #do 
 #    echo $i
 #done

function rmnot {
#removes all things except those in list. The algorithm is very inefficient
#and very clever.
   patternSpace=""
   fileSpace=`ls`
   for i in "$@"
   do
       patternSpace=$patternSpace\ $(ls | grep -v $i)
   done   
   
   itemPos=0
   itemCount=0
   maxItemCount=0
   for item in $fileSpace
   do
	   for element in $patternSpace
           do
		   if [ $element == $item ]; then
			   itemCount=$(($itemCount+1))
	           fi
	   done
     if [ $itemCount -gt $maxItemCount ]; then
        maxItemCount=$itemCount	   
     fi
     
   itemStruct[$itemPos]=$itemCount
   itemPos=$(($itemPos+1))
   itemCount=0
   done

   deleteCount=0
   for deleteCandidate in $fileSpace
   do    
       if [[ ${itemStruct[$deleteCount]} -eq $maxItemCount ]]; then
	    #The following line hinges on element i in itemStruct
	    #corresponding to element i in fileSpace
	    echo Thing to Delete: $deleteCandidate
	    rm $deleteCandidate
       fi
       deleteCount=$(($deleteCount+1))
   done
   #items that occur each time are to be deleted
}

                      #Beyond this point is Debian Default

# If not running interactively, don't do anything
[ -z "$PS1" ] && return


# check the window size after each command and, if necessary,
# update the values of LINES and COLUMNS.
shopt -s checkwinsize

# make less more friendly for non-text input files, see lesspipe(1)
#[ -x /usr/bin/lesspipe ] && eval "$(SHELL=/bin/sh lesspipe)"

# set variable identifying the chroot you work in (used in the prompt below)
if [ -z "$debian_chroot" ] && [ -r /etc/debian_chroot ]; then
    debian_chroot=$(cat /etc/debian_chroot)
fi

# set a fancy prompt (non-color, unless we know we "want" color)
case "$TERM" in
    xterm-color) color_prompt=yes;;
esac

# uncomment for a colored prompt, if the terminal has the capability; turned
# off by default to not distract the user: the focus in a terminal window
# should be on the output of commands, not on the prompt
#force_color_prompt=yes

if [ -n "$force_color_prompt" ]; then
    if [ -x /usr/bin/tput ] && tput setaf 1 >&/dev/null; then
	# We have color support; assume it's compliant with Ecma-48
	# (ISO/IEC-6429). (Lack of such support is extremely rare, and such
	# a case would tend to support setf rather than setaf.)
	color_prompt=yes
    else
	color_prompt=
    fi
fi

if [ "$color_prompt" = yes ]; then
    PS1='${debian_chroot:+($debian_chroot)}\[\033[01;32m\]\u@\h\[\033[00m\]:\[\033[01;34m\]\w\[\033[00m\]\$ '
else
    PS1='${debian_chroot:+($debian_chroot)}\u@\h:\w\$ '
fi
unset color_prompt force_color_prompt

# If this is an xterm set the title to user@host:dir
case "$TERM" in
xterm*|rxvt*)
    PS1="\[\e]0;${debian_chroot:+($debian_chroot)}\u@\h: \w\a\]$PS1"
    ;;
*)
    ;;
esac

# enable color support of ls and also add handy aliases
if [ -x /usr/bin/dircolors ]; then
    test -r ~/.dircolors && eval "$(dircolors -b ~/.dircolors)" || eval "$(dircolors -b)"
    alias ls='ls --color=auto'
    #alias dir='dir --color=auto'
    #alias vdir='vdir --color=auto'

    #alias grep='grep --color=auto'
    #alias fgrep='fgrep --color=auto'
    #alias egrep='egrep --color=auto'
fi

# Alias definitions.
# You may want to put all your additions into a separate file like
# ~/.bash_aliases, instead of adding them here directly.
# See /usr/share/doc/bash-doc/examples in the bash-doc package.

if [ -f ~/.bash_aliases ]; then
    . ~/.bash_aliases
fi

# enable programmable completion features (you don't need to enable
# this, if it's already enabled in /etc/bash.bashrc and /etc/profile
# sources /etc/bash.bashrc).
if [ -f /etc/bash_completion ] && ! shopt -oq posix; then
    . /etc/bash_completion
fi
