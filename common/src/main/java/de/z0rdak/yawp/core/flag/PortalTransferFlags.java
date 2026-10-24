package de.z0rdak.yawp.core.flag;

import net.minecraft.server.level.ServerPlayer;

/**
 * Maintains temporary context identifying portal-triggered transfers.
 *
 * <p>The context is thread-local because portal processing and the associated
 * teleport checks execute synchronously on the server thread. This avoids
 * storing transient transfer state in persistent player data.</p>
 *
 * <p>The previous context must be restored after each operation to support
 * nested calls without accidentally clearing an outer transfer's context.</p>
 */
public final class PortalTransferFlags {

    /**
     * The server player whose portal transfer is currently being processed
     * on this thread, or {@code null} if there is no active portal transfer.
     */
    private static final ThreadLocal<ServerPlayer> CURRENT = new ThreadLocal<>();

    private PortalTransferFlags() {}


    /**
     * Checks whether the specified player is currently inside a
     * portal-triggered transfer operation.
     *
     * @param player the player to check
     * @return {@code true} if this player's portal-transfer context is active
     */
    public static boolean isPortalTransfer(ServerPlayer player) {
        return CURRENT.get() == player;
    }

    /**
     * Enters portal-transfer context for the specified player.
     *
     * @param player the player whose transfer is being processed
     * @return the previous context, which must be restored afterward
     */
    public static ServerPlayer enter(ServerPlayer player) {
        ServerPlayer previous = CURRENT.get();
        CURRENT.set(player);
        return previous;
    }

    /**
     * Restores the context that was active before entering a portal transfer.
     *
     * @param previous the context returned by {@link #enter(ServerPlayer)}
     */
    public static void exit(ServerPlayer previous) {
        if (previous == null) {
            CURRENT.remove();
        } else {
            CURRENT.set(previous);
        }
    }
}