struct message { int msglen; char* msgptr; };

int foo(struct message* mptr) {
    return mptr->msglen;
}
