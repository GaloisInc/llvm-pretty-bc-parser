extern int global_var;

extern struct s { int fld1; char fld2; } global_struct;

int foo(void) { return global_var; }

struct s* bar(void) { return &global_struct; }
