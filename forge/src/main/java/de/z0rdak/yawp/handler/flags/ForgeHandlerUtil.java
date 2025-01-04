package de.z0rdak.yawp.handler.flags;

import de.z0rdak.yawp.handler.HandlerUtil;
import net.minecraftforge.event.entity.EntityEvent;
import net.minecraftforge.event.entity.player.PlayerEvent;
import net.minecraftforge.event.level.BlockEvent;

public class ForgeHandlerUtil {

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
