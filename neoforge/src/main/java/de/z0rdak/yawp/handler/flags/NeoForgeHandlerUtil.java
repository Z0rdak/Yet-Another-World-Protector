package de.z0rdak.yawp.handler.flags;

import de.z0rdak.yawp.handler.HandlerUtil;
import net.neoforged.neoforge.event.entity.EntityEvent;
import net.neoforged.neoforge.event.entity.player.PlayerEvent;
import net.neoforged.neoforge.event.level.BlockEvent;

public class NeoForgeHandlerUtil {

    public static boolean isServerSide(EntityEvent event) {
        return HandlerUtil.isServerSide(event.getEntity());
    }

    public static boolean isServerSide(BlockEvent event) {
        return HandlerUtil.isServerSide(event.getLevel());
    }

    public static boolean notServerSideOrPlayerNull(PlayerEvent event) {
        return !isServerSide(event) || event.getEntity() == null;
    }
}
