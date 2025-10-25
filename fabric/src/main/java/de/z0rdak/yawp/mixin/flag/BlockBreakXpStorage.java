package de.z0rdak.yawp.mixin.flag;

import net.minecraft.server.level.ServerPlayer;

import java.util.HashMap;
import java.util.Map;
import java.util.UUID;

public class BlockBreakXpStorage {
    private static final Map<UUID, Integer> XP_MAP = new HashMap<>();

    public static void setXpForNextBreak(ServerPlayer player, int xp) {
        XP_MAP.put(player.getUUID(), xp);
    }

    public static int consumeXp(ServerPlayer player) {
        if (!XP_MAP.containsKey(player.getUUID())) {
            return 0;
        }
        var refundXp = XP_MAP.get(player.getUUID());
        XP_MAP.remove(player.getUUID());
        return refundXp;
    }
}
