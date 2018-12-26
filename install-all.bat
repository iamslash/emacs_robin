@ECHO OFF

IF NOT EXIST %HOME% GOTO _ERROR_NOT_SET_HOME_ENV

REM default variables
SET EMACS_ROBIN_HOME=%HOME%\.emacs_robin
SET EMACS_FILE=%HOME%\.emacs

REM validate arguments
IF EXIST %EMACS_FILE% GOTO _ERROR_ALREADY_EXIST_EMACS_FILE
IF EXIST %EMACS_ROBIN_HOME% GOTO _ERROR_ALREADY_EXIST_EMACS_ROBIN_HOME

REM clone emacs_robin
git clone git@github.com:iamslash/emacs_robin.git %EMACS_ROBIN_HOME%
REM create .emacs
ECHO (load (concat (getenv "HOME") "\\.emacs_robin\\emacs.el")) > %EMACS_FILE%
REM mklink %EMACS_FILE% %EMACS_ROBIN_HOME%\.emacs


ECHO Success to install emacs_robin
GOTO _END

:_ERROR_NOT_SET_HOME_ENV
ECHO ERROR)
ECHO   Not set HOME env
GOTO _END

:_ERROR_ALREADY_EXIST_EMACS_FILE
ECHO ERROR)
ECHO   Already exist .emacs
GOTO _END

:_ERROR_ALREADY_EXIST_EMACS_ROBIN_HOME
ECHO ERROR)
ECHO   Already exist .emacs_robin
GOTO _END

:_END
REM ECHO %EMACS_FILE%
