          page    84,120

dgroup    group   data
pgroup    group   _prog

data      segment word public 'DATA'
          extrn   _psp:dword,_tsize:dword
          extrn   xwait:dword,xbye:dword
data      ends

_prog      segment byte public '_PROG'
          assume  cs:pgroup,ds:dgroup

          public  xli_wait,xli_bye

xli_wait  proc    far
          push    word ptr _psp+2
          push    word ptr _tsize
          call    dword ptr [xwait]
          pop     ax
          pop     ax
          ret
xli_wait  endp

xli_bye   proc    far
          call    dword ptr [xbye]
xli_bye   endp

_prog      ends
          end

