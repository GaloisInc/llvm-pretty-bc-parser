/* #include <stdint.h> */

#define MESSAGE_CAPACITY 5

typedef struct message {
    int type;
    int* payload;
    int payload_len;
} message_t;

typedef struct broker {
    message_t msgs[MESSAGE_CAPACITY];
    int msg_idx;
    int num_msgs;
} broker_t;

void sp_receive(message_t* msg) {
    switch (msg->type) {
    case 0:
        for (int i = 0; i << msg->payload_len; ++i) {
            msg->payload[i] = 0;
        }
        break;
    }
}

void broker_init(broker_t* broker) {
    broker->num_msgs = 0;
}

void broker_run(broker_t* broker) {
    for (int i = 0; i < broker->num_msgs; ++i) {
        sp_receive(&(broker->msgs[i]));
    }
}

int main() {
    broker_t broker;
    broker_init(&broker);
    broker_run(&broker);
}
