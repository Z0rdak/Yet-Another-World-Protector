package de.z0rdak.yawp.handler.flags;

import net.fabricmc.fabric.api.event.lifecycle.v1.ServerTickEvents;
import net.minecraft.server.MinecraftServer;

import java.util.ArrayList;
import java.util.List;

/**
 * Schedules server-thread tasks to run after a specified number of server ticks.
 *
 * <p>This scheduler is used to defer block-entity update packets when a block
 * break is denied by server-side protection. The client may predictively remove
 * a block before the server rejects the break, causing block-entity update
 * packets sent immediately to be ignored because the client no longer has the
 * corresponding block entity.
 *
 * <p>Delaying the update gives the client time to reconcile its predicted block
 * state before receiving the block-entity data. This is a workaround for the
 * client-side prediction behavior; it does not affect the server's authoritative
 * block state or the protection decision.
 */
public final class DelayedServerTasks {
    private record Task(long targetTick, Runnable action) {}

    private static final List<Task> TASKS = new ArrayList<>();
    private static long currentTick;

    private DelayedServerTasks() {}

    public static void initialize() {
        ServerTickEvents.END_SERVER_TICK.register(server -> {
            currentTick++;

            List<Task> ready = new ArrayList<>();
            TASKS.removeIf(task -> {
                if (task.targetTick() <= currentTick) {
                    ready.add(task);
                    return true;
                }
                return false;
            });

            for (Task task : ready) {
                task.action().run();
            }
        });
    }

    public static void schedule(long delayTicks, Runnable action) {
        TASKS.add(new Task(currentTick + delayTicks, action));
    }
}
