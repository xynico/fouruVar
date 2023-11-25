@echo off
if "%1"=="" (
  echo Usage: %0 hostname
  exit /b
)
ssh %1 -t "tmux attach || tmux new-session"