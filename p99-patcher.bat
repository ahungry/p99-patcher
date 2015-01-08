@echo off
start "patcher" /min p99-patcher.exe --eval "(p99-patcher:main)"
start "browser" /max http://localhost:4444