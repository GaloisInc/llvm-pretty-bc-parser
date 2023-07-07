#include <stdint.h>


#define MESSAGE_CAPACITY 10
#define SIGNAL_LEN 200

static const uint16_t MSG_TYPE_CLEAR = 0;
static const uint16_t MSG_TYPE_ADD_FIVE = 1;
static const uint16_t MSG_TYPE_DOUBLE = 2;

extern uint8_t signal[SIGNAL_LEN]; /* = {0}; */

typedef struct message {
    uint16_t type;
    uint8_t * payload;
    uint8_t payload_len;
} message_t;

typedef struct broker {
    message_t msgs[MESSAGE_CAPACITY];
    uint8_t msg_idx;
    uint8_t num_messages;
} broker_t;

extern broker_t broker;

void sp_receive(message_t * msg);

void broker_init(broker_t * broker) {
    broker->num_messages = 0;
}

void broker_run(broker_t * broker) {
    for (uint8_t i = 0; i < broker->num_messages; i++) {
        // direct call
        sp_receive(&(broker->msgs[i]));
    }
}

void broker_publish(broker_t * broker, message_t * msg) {
    broker->msgs[broker->num_messages++] = *msg;
}

void sp_clear(uint8_t * signal, uint8_t signal_len) {
    for (uint8_t i = 0; i < signal_len; i++) {
        signal[i] = 0;
    }
}

void sp_double(uint8_t * signal, uint8_t signal_len) {
    for (uint8_t i = 0; i < signal_len; i++) {
        signal[i] *= 2;
    }
}

void sp_add_five(uint8_t * signal, uint8_t signal_len) {
    for (uint8_t i = 0; i < signal_len; i++) {
        signal[i] += 5;
    }
}

void sp_send(uint8_t * signal) {
    message_t msg = {MSG_TYPE_CLEAR, signal, SIGNAL_LEN};
    broker_publish(&broker, &msg);
}

void sp_receive(message_t * msg) {
    switch (msg->type) {
        case MSG_TYPE_CLEAR:
            sp_clear(msg->payload, msg->payload_len);
            break;
        case MSG_TYPE_ADD_FIVE:
            sp_add_five(msg->payload, msg->payload_len);
            break;
        case MSG_TYPE_DOUBLE:
            sp_double(msg->payload, msg->payload_len);
            break;
        default:
            break;
    }
}

int main() {
    broker_init(&broker);

    message_t msg = {MSG_TYPE_CLEAR, signal, SIGNAL_LEN};
    broker_publish(&broker, &msg);
    msg.type = MSG_TYPE_ADD_FIVE;
    broker_publish(&broker, &msg);
    msg.type = MSG_TYPE_DOUBLE;
    broker_publish(&broker, &msg);

    broker_run(&broker);
}
