          page    84,120

dgroup    group   _DATA
pgroup    group   _TEXT

_DATA     segment word public 'DATA'
_DATA     ends

          extrn   __psp:word,_tsize:word
          extrn   _xwait:dword,_xbye:dword
 
_TEXT     segment byte public 'CODE'
          assume  cs:pgroup,ds:dgroup

          public  _xli_wait,_xli_bye

_xli_wait proc    near
          push    __psp
          push    _tsize
          call    dword ptr [_xwait]
          pop     ax
          pop     ax
          ret
_xli_wait endp

_xli_bye  proc    near
          call    dword ptr [_xbye]
_xli_bye  endp

_TEXT     ends
          end

