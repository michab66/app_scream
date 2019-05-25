          page    84,120

dgroup    group   data
pgroup    group   prog

data      segment word public 'DATA'
data      ends

prog      segment byte public 'PROG'
          assume  cs:pgroup,ds:dgroup

          extrn   _psp:word,_tsize:word
          extrn   xwait:dword,xbye:dword
          public  xli_wait,xli_bye

xli_wait  proc    near
          push    _psp+2
          push    _tsize
          call    dword ptr [xwait]
          pop     ax
          pop     ax
          ret
xli_wait  endp

xli_bye   proc    near
          call    dword ptr [xbye]
xli_bye   endp

prog      ends
          end

