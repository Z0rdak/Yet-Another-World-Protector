package de.z0rdak.yawp.core.trigger;

public record RegionTrigger(String id, String type, boolean inOurOut, boolean enabled) {

    /*
    Trigger strategies registering for different triggers

    - Command holds command info, CommandTrigger holds logic to execute it
    - Title Message holds title info, TitleTrigger holds logic to execute it
      (Actually TitleTrigger could use CommandTrigger under the hood)
    -
     */

}
