entry main

@main 0 main
main:
	const r0, 5
	!trace
	call r1, loop_factorial, r0
	!untrace
	ret r1

@recursive_factorial 1
check_base_case:
	const r1, 2
	const r2, 1
	bl r0, r1, return
	branch main_case
main_case:
	sub r3, r0, r2
	call r4, recursive_factorial, r3
	mul r5, r4, r0
	branch return
return:
	phi r6, main_case, r5, r2
	ret r6

@loop_factorial 1
init_loop:
	const r1, 1
	branch loop_body
loop_body:
	phi r2, init_loop, r1, r4
	phi r3, init_loop, r0, r5
	mul r4, r2, r3
	sub r5, r3, r1
	bl r5, r1, return
	branch loop_body
return:
	ret r2
