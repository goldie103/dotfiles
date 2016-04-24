# vim:ft=zsh ts=2 sw=2 sts=2

DEFAULT_HOST="kelly-ThinkPad-X130e"
DEFAULT_USER="kelly"
CURRENT_BG='NONE'

f() { echo -n "%{%F{$1}%}" }
b() { echo -n "%{%K{$1}%}" }

add() {
	local s=""
	# fg
	[[ -n $2 ]] && s+="$(f $2)"
	# bg
	[[ -n $3 ]] && s+="$(b $3)"
	# text
	[[ -n $1 ]] && s+=$1
	echo -n $s
}

add_venv() { [[ -n $VIRTUAL_ENV && -n $VIRTUAL_ENV_DISABLE_PROMPT ]] && add "($(basename $VIRTUAL_ENV))" }

add_context() {
	local s
	[[ $USER != $DEFAULT_USER && $USER != "root" || -n $SSH_CLIENT ]] && s+="%n"
	[[ $HOST != $DEFAULT_HOST || -n $SSH_CLIENT ]] && s+="@%m"
	[[ -n $s ]] && add "$s > "
}

ZSH_THEME_GIT_PROMPT_PREFIX=" $(f magenta)"
ZSH_THEME_GIT_PROMPT_SUFFIX="%{$reset_color%}"

add_git() {
	local branch, upstream_status, is_dirty;
	local st="$(git status 2>/dev/null)"
	if [[ -n $st ]]; then
		arr=(${(f)st})

		[[ $arr[1] =~ 'Not currently on any branch.' ]] && branch='no branch' || branch="${arr[1][(w)4]}"

		if [[ $arr[2] =~ 'Your branch is' ]]; then
			if [[ $arr[2] =~ 'ahead' ]]; then
				upstream_status='ahead'
			elif [[ $arr[2] =~ 'diverged' ]]; then
				upstream_status='diverged'
			else
				upstream_status='behind'
			fi
		fi

		[[ ! $st =~ 'nothing to commit' ]] && is_dirty='1'
}

add_status () {
  local s
	# root
  [[ $UID -eq 0 ]] && s+="$(add ⚡ yellow)"
	# background jobs
  [[ $(jobs -l | wc -l) -gt 0 ]] && s+="$(add ⚙ cyan)"
	# error
  [[ $RETVAL -ne 0 ]] && s+="$(add $ red)"

  [[ -n "$symbols" ]] && echo -n "$symbols" || echo -n "%{$reset_color%}$"
}


PROMPT=' $(build_prompt)'

build_prompt() {
	RETVAL=$?
	add_venv
	add_context
	add "%~" blue
	add_git
	add " "
	add_status
	add "%{$reset_color%} "
}



