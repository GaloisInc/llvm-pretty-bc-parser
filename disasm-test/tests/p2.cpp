/* This is the p1 demo but modified to make uorb a class and the uorb
 * interaction via methods and not functions.
 */

#include <stdlib.h>


#define UORB_DEFINE(name)                       \
  const struct orb_metadata name = { #name };

struct orb_metadata {
  char * name;
};

/* uORB */

class uorb {
public:

   /* Subscribe to a topic described by the metadata
      
      Returns a handle (file descriptor) that can be polled to
      determine when messages are available
   */
    int uorb_subscribe(const struct orb_metadata * o) { return 1; }

    /* Unsubscribe from the given topic */
    void uorb_unsubscribe(const struct orb_metadata * o) {}

    /* Atomically publish data to the topic */
    int uorb_publish(const struct orb_metadata * o, const void *data) { return 0; }
    
    /* Copy a message off of the handle returned by `uorb_subscribe` */
    int uorb_copy(const struct orb_metadata * o, int handle, void *buffer) { return 0; }
    
    /* Declare that this component publishes to this channel */
    int uorb_advertise(const struct orb_metadata * o) { return 0; }
}
    main_uorb;

UORB_DEFINE(vehicle_gps);
UORB_DEFINE(gyro);
UORB_DEFINE(battery_status);
UORB_DEFINE(vehicle_status);
UORB_DEFINE(manual_setpoint);
UORB_DEFINE(actuator_controls);
UORB_DEFINE(angular_velocity);
UORB_DEFINE(trigger);
UORB_DEFINE(camera_capture);

/* Vehicle GPS */
struct gps_status {
  double lat;
  double lon;
};

void vehicle_gps_start() {
  main_uorb.uorb_advertise(&vehicle_gps);
}

void vehicle_gps_update() {
  struct gps_status stat;
  main_uorb.uorb_publish(&vehicle_gps, &stat);
}

void vehicle_gps_stop() {}

/* Vehicle Angular Acceleration */
struct angular_accel {
  double accel;
};
int vehicle_angular_acc_handle;
void vehicle_angular_acc_start() {
  vehicle_angular_acc_handle = main_uorb.uorb_subscribe(&gyro);
  main_uorb.uorb_advertise(&angular_velocity);
}

void vehicle_angular_acc_stop() {
  main_uorb.uorb_unsubscribe(&gyro);
}

struct gyro_status {
  double angleX;
  double angleY;
  double angleZ;
};

void compute_angular_accel(struct gyro_status *stat, struct angular_accel *msg) {
  msg->accel = 100 * stat->angleX + stat->angleY - stat->angleZ;
}

// The vehicle angular accelerator update function that is called in the periodic update loop
void vehicle_angular_acc_update() {
  // Pull inputs from the message bus
  struct gyro_status stat;
  main_uorb.uorb_copy(&gyro, vehicle_angular_acc_handle, &stat);

  // Compute a message to send based on those inputs
  struct angular_accel msg;
  compute_angular_accel(&stat, &msg);
  main_uorb.uorb_publish(&angular_velocity, &msg);
}

/* Gyro Sensor */
void gyro_start() {
  main_uorb.uorb_advertise(&gyro);
}

void read_gyro(struct gyro_status *stat) {
  volatile double * getX = (double*)0x555550;
  volatile double * getY = (double*)0x555560;
  volatile double * getZ = (double*)0x555570;
  stat->angleX = *getX;
  stat->angleY = *getY;
  stat->angleZ = *getZ;
}

int gyro_calc_inconsistency(struct gyro_status *stat) {
  if(rand() < 100) return 1;
  else return 0;
}

void gyro_update() {
  struct gyro_status stat;
  read_gyro(&stat);
  if(!gyro_calc_inconsistency(&stat))
    main_uorb.uorb_publish(&gyro, &stat);
}


/* Fixedwing Attitude Control */
struct actuator_control {
  int mode;
  double pitch;
  double yaw;
};

int fw_att_control_handle;
void fw_att_control_start() {
  fw_att_control_handle = main_uorb.uorb_subscribe(&angular_velocity);
  main_uorb.uorb_advertise(&actuator_controls);
}

void compute_att_control(struct angular_accel *vel, struct actuator_control *msg) {
  msg->mode = 1;
  msg->pitch = vel->accel;
  msg->yaw = vel->accel * 10;
}

void fw_att_control_update() {
  struct angular_accel vel;
  main_uorb.uorb_copy(&angular_velocity, fw_att_control_handle, &vel);

  struct actuator_control msg;
  compute_att_control(&vel, &msg);
  main_uorb.uorb_publish(&actuator_controls, &msg);
}


/* Camera */
void cam_init() {
  main_uorb.uorb_subscribe(&trigger);
}

struct captured_image {
  char buffer[100];
};

void cam_capture() {
  struct captured_image img;
  main_uorb.uorb_publish(&camera_capture, &img);
}

/* Airship Attitude Control */
int airship_att_handle1;
int airship_att_handle2;
int airship_att_handle3;
void airship_att_start() {
  airship_att_handle1 = main_uorb.uorb_subscribe(&vehicle_status);
  airship_att_handle2 = main_uorb.uorb_subscribe(&angular_velocity);
  airship_att_handle3 = main_uorb.uorb_subscribe(&manual_setpoint);
}

void airship_att_stop() {
  main_uorb.uorb_unsubscribe(&vehicle_status);
  main_uorb.uorb_unsubscribe(&angular_velocity);
  main_uorb.uorb_unsubscribe(&manual_setpoint);
}

struct vehicle_status {
  int on_fire;
};

struct setpoint {
  int x;
};
void airship_att_update() {
  struct vehicle_status stat;
  main_uorb.uorb_copy(&vehicle_status, airship_att_handle1, &stat);
  struct angular_accel vel;
  main_uorb.uorb_copy(&angular_velocity, airship_att_handle2, &vel);
  struct setpoint sp;
  main_uorb.uorb_copy(&manual_setpoint, airship_att_handle3, &sp);

  cam_capture();
  if(rand() < 5) {
    airship_att_stop();
  }
}

struct gyro_recalibrate { int tbd; };
struct gyro_confirmed { bool confirmed; };
struct gyro_alert_msg { bool out_of_range; };

void airship_att_recalibrate() {
    struct gyro_status stat;
    main_uorb.uorb_copy(&gyro, vehicle_angular_acc_handle, &stat);
    if (stat.angleZ > 100) {
        struct gyro_recalibrate rmsg;
        struct gyro_confirmed cmsg = { .confirmed = false };
        for (int i = 0; i < 5 && !cmsg.confirmed; ++i) {
            main_uorb.uorb_publish(&gyro, &rmsg);
            main_uorb.uorb_copy(&gyro, vehicle_angular_acc_handle, &cmsg);
        }
        if (!cmsg.confirmed) {
            struct gyro_alert_msg amsg = { .out_of_range = true };
            main_uorb.uorb_publish(&angular_velocity, &amsg);
        }
    } else {
        struct angular_accel msg;
        compute_angular_accel(&stat, &msg);
        main_uorb.uorb_publish(&angular_velocity, &msg);
    }
}

/* Storage */
void px4_log() {
  main_uorb.uorb_subscribe(&vehicle_status);
  main_uorb.uorb_subscribe(&battery_status);
}

void subscribe_all() {
  vehicle_angular_acc_start();
  vehicle_gps_start();
  fw_att_control_start();
  airship_att_start();
  gyro_start();
}

void run_control_cycle() {
  vehicle_gps_update();
  vehicle_angular_acc_update();
  gyro_update();
  fw_att_control_update();
  airship_att_update();
  airship_att_recalibrate();
}

int main(int argc, char* argv[]) {
  /* First, set up all of the subscriptions */
  subscribe_all();

  while(1) {
    run_control_cycle();
  }

  return 0;
}
