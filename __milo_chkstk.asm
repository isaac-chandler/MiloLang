.code

__chkstk proc frame
	push   rcx
.pushreg rcx
	push   rax
.pushreg rax
.endprolog
	cmp    rax, 1000h
	lea    rcx, [rsp + 24]
	jb     skip
top:
	sub    rcx, 1000h
	test   [rcx],rcx
	sub    rax, 1000h
	cmp    rax, 1000h
	ja     top
skip:
	sub    rcx, rax
	test   [rcx], rcx
	pop    rax
	pop rcx
	ret
__chkstk endp
	
end