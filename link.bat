IF NOT EXIST "%USERPROFILE%\AppData\Local\nvim\" md %USERPROFILE%\AppData\Local\nvim
mklink %USERPROFILE%\AppData\Local\nvim\init.vim %DOT%\init.vim
pause
