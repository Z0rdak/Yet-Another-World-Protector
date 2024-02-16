package de.z0rdak.yawp.handler.flags;

import de.z0rdak.yawp.core.flag.IFlag;
import de.z0rdak.yawp.core.flag.RegionFlag;
import de.z0rdak.yawp.core.region.DimensionalRegion;
import de.z0rdak.yawp.core.region.IMarkableRegion;
import de.z0rdak.yawp.core.region.IProtectedRegion;
import de.z0rdak.yawp.util.LocalRegions;
import de.z0rdak.yawp.util.MessageUtil;
import net.minecraft.ChatFormatting;
import net.minecraft.core.BlockPos;
import net.minecraft.network.chat.MutableComponent;
import net.minecraft.network.chat.TextComponent;
import net.minecraft.resources.ResourceKey;
import net.minecraft.world.entity.Entity;
import net.minecraft.world.entity.FlyingMob;
import net.minecraft.world.entity.animal.Animal;
import net.minecraft.world.entity.animal.WaterAnimal;
import net.minecraft.world.entity.boss.enderdragon.EnderDragon;
import net.minecraft.world.entity.monster.Monster;
import net.minecraft.world.entity.monster.Shulker;
import net.minecraft.world.entity.monster.Slime;
import net.minecraft.world.entity.npc.AbstractVillager;
import net.minecraft.world.entity.player.Player;
import net.minecraft.world.level.Level;
import net.minecraftforge.event.entity.EntityEvent;
import net.minecraftforge.event.world.BlockEvent;
import net.minecraftforge.eventbus.api.Event;

import javax.annotation.Nullable;
import java.util.Map;

public final class HandlerUtil {

    private HandlerUtil() {
    }

    public static ResourceKey<Level> getEntityDim(Entity entity) {
        return entity.getCommandSenderWorld().dimension();
    }

    public static boolean isAnimal(Entity entity) {
        return entity instanceof Animal || entity instanceof WaterAnimal;
    }

    public static boolean isServerSide(EntityEvent event) {
        return isServerSide(event.getEntity());
    }

    public static boolean isServerSide(BlockEvent event) {
        return !event.getWorld().isClientSide();
    }

    public static boolean isServerSide(Entity entity) {
        return !entity.getCommandSenderWorld().isClientSide;
    }

    public static boolean isVillager(Entity entity) {
        return entity instanceof AbstractVillager;
    }

    public static boolean isPlayer(Entity entity) {
        return entity instanceof Player;
    }

    public static boolean isMonster(Entity entity) {
        return entity instanceof Monster
                || entity instanceof Slime
                || entity instanceof FlyingMob
                || entity instanceof EnderDragon
                || entity instanceof Shulker;
    }

    public static FlagCheckEvent checkEvent(BlockPos target, RegionFlag regionFlag, DimensionalRegion dimRegion, @Nullable Player player) {
        IMarkableRegion involvedRegion = LocalRegions.getInvolvedRegionFor(target, dimRegion.getDim());
        FlagCheckEvent flagCheck = new FlagCheckEvent(dimRegion, involvedRegion, regionFlag);
        if (player != null) {
            // TODO: Make this lazy and/or only execute it when msg is to be displayed
            // FIXME: currently the dim name is set here as {region}, and is corrected later
            Map<String, String> subsMap = FlagMessageUtil.defaultSubstitutes(regionFlag, dimRegion, target, player);
            flagCheck = new PlayerFlagEvent(flagCheck, player, subsMap);
        }
        if (involvedRegion == null) {
            flagCheck.setDeniedLocal(false);
        } else {
            boolean isDeniedLocally = checkDeniedInRegion(involvedRegion, regionFlag, player);
            flagCheck.setDeniedLocal(isDeniedLocally);
        }
        boolean isDeniedInDim = checkDeniedInRegion(dimRegion, regionFlag, player);
        flagCheck.setDeniedInDim(isDeniedInDim);
        return getCheckResult(involvedRegion, flagCheck);
    }

    public static FlagCheckEvent checkEvent(BlockPos target, RegionFlag regionFlag, DimensionalRegion dimRegion) {
        return checkEvent(target, regionFlag, dimRegion, null);
    }

    private static boolean checkDeniedInRegion(IProtectedRegion region, RegionFlag regionFlag, @Nullable Player player) {
        if (region.isActive()) {
            boolean disallowsPlayer = player == null || !region.permits(player);
            if (region.containsFlag(regionFlag) && disallowsPlayer) {
                IFlag flag = region.getFlag(regionFlag.name);
                // TODO: Check state with allowed
                return flag.isActive();
            } else {
                return false;
            }
        } else {
            return false;
        }
    }

    private static FlagCheckEvent getCheckResult(IMarkableRegion involvedRegion, FlagCheckEvent flagCheck) {
        if (flagCheck.getLocalRegion() == null) {
            flagCheck.setDenied(flagCheck.isDeniedInDim());
        } else {
            boolean isDenied = flagCheck.isDeniedInDim() && flagCheck.isDeniedLocal()
                    || !flagCheck.isDeniedInDim() && flagCheck.isDeniedLocal()
                    || flagCheck.isDeniedInDim() && involvedRegion == null;
            flagCheck.setDenied(isDenied);
        }
        return flagCheck;
    }

    /**
     * Handles the given flag check event and sends the flag message if the flag is not muted <br>
     *
     * @param event     the flag check event to handle and send the message for
     * @param flagCheck the flag check event to handle and send the message for
     * @return true if the flag is denied, else false
     */
    public static boolean handleAndSendMsg(Event event, FlagCheckEvent flagCheck) {
        FlagMessageUtil.sendFlagMsg(flagCheck);
        event.setCanceled(flagCheck.isDenied());
        return flagCheck.isDenied();
    }
}
