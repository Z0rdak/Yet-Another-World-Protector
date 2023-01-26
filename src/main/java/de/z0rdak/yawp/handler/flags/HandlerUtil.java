package de.z0rdak.yawp.handler.flags;

import de.z0rdak.yawp.core.flag.IFlag;
import de.z0rdak.yawp.core.flag.RegionFlag;
import de.z0rdak.yawp.core.region.DimensionalRegion;
import de.z0rdak.yawp.core.region.IMarkableRegion;
import de.z0rdak.yawp.util.MessageUtil;
import net.minecraft.core.BlockPos;
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
import net.minecraftforge.event.level.BlockEvent;
import net.minecraftforge.eventbus.api.Event;

import static de.z0rdak.yawp.util.LocalRegions.getInvolvedRegionFor;

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
        return !event.getLevel().isClientSide();
    }

    public static boolean isServerSide(Entity entity) {
        return !entity.getCommandSenderWorld().isClientSide();
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

    public static boolean handleAndSendMsg(Event event, FlagCheckEvent.PlayerFlagEvent flagCheck) {
        if (flagCheck.getLocalRegion() == null && flagCheck.isDeniedInDim()) {
            MessageUtil.sendDimFlagNotification(flagCheck.getPlayer(), flagCheck.getFlag());
        }
        if (flagCheck.isDeniedLocal()) {
            if (!flagCheck.getLocalRegion().isMuted()) {
                MessageUtil.sendFlagNotification(flagCheck.getPlayer(), flagCheck.getLocalRegion(), flagCheck.getFlag());
            }
        }
        event.setCanceled(flagCheck.isDenied());
        return flagCheck.isDenied();
    }

    public static boolean sendFlagDeniedMsg(FlagCheckEvent.PlayerFlagEvent flagCheck) {
        if (flagCheck.getLocalRegion() == null && flagCheck.isDeniedInDim()) {
            // TODO: Muted property for dimensions | config option | don't display ?
            MessageUtil.sendDimFlagNotification(flagCheck.getPlayer(), flagCheck.getFlag());
        }
        if (flagCheck.isDeniedLocal()) {
            if (!flagCheck.getLocalRegion().isMuted()) {
                MessageUtil.sendFlagNotification(flagCheck.getPlayer(), flagCheck.getLocalRegion(), flagCheck.getFlag());
            }
        }
        return flagCheck.isDenied();
    }

    public static boolean sendFlagDeniedMsg(FlagCheckEvent flagCheck, Player player) {
        if (flagCheck.getLocalRegion() == null && flagCheck.isDeniedInDim()) {
            // TODO: Muted property for dimensions | config option | don't display ?
            MessageUtil.sendDimFlagNotification(player, flagCheck.getFlag());
        }
        if (flagCheck.isDeniedLocal()) {
            if (!flagCheck.getLocalRegion().isMuted()) {
                MessageUtil.sendFlagNotification(player, flagCheck.getLocalRegion(), flagCheck.getFlag());
            }
        }
        return flagCheck.isDenied();
    }

    public static FlagCheckEvent.PlayerFlagEvent checkPlayerEvent(Player player, BlockPos target, RegionFlag regionFlag, DimensionalRegion dimRegion) {
        IMarkableRegion involvedRegion = getInvolvedRegionFor(regionFlag, target, player, player.level.dimension());
        FlagCheckEvent.PlayerFlagEvent flagCheck = new FlagCheckEvent.PlayerFlagEvent(player, dimRegion, involvedRegion, regionFlag);
        if (involvedRegion == null) {
            flagCheck.setDeniedLocal(false);
        } else {
            IFlag flag = involvedRegion.getFlag(regionFlag.name);
            // TODO: Check state with allowed
            flagCheck.setDeniedLocal(flag.isActive());
        }
        if (dimRegion.isActive()) {
            if (dimRegion.containsFlag(regionFlag) && !dimRegion.permits(player)) {
                IFlag flag = dimRegion.getFlag(regionFlag.name);
                // TODO: Check state with allowed
                flagCheck.setDeniedInDim(flag.isActive());
            } else {
                flagCheck.setDeniedInDim(false);
            }
        } else {
            flagCheck.setDeniedInDim(false);
        }

        if (flagCheck.getLocalRegion() == null) {
            flagCheck.setDenied(flagCheck.isDeniedInDim());
            return flagCheck;
        } else {
            boolean deniedResult = flagCheck.isDeniedInDim() && flagCheck.isDeniedLocal() || (!flagCheck.isDeniedInDim() || flagCheck.isDeniedLocal()) && (!flagCheck.isDeniedInDim() && (flagCheck.isDeniedLocal()) || !flagCheck.isDeniedInDim() && !flagCheck.isDeniedLocal());
            flagCheck.setDenied(deniedResult);
            return flagCheck;
        }
    }

    public static FlagCheckEvent checkTargetEvent(BlockPos target, RegionFlag regionFlag, DimensionalRegion dimRegion) {
        IMarkableRegion involvedRegion = getInvolvedRegionFor(regionFlag, target, dimRegion.getDim());
        FlagCheckEvent flagCheck = new FlagCheckEvent(dimRegion, involvedRegion, regionFlag);
        if (involvedRegion == null) {
            flagCheck.setDeniedLocal(false);
        } else {
            IFlag flag = involvedRegion.getFlag(regionFlag.name);
            // TODO: Check state with allowed
            flagCheck.setDeniedLocal(flag.isActive());
        }
        if (dimRegion.isActive()) {
            if (dimRegion.containsFlag(regionFlag)) {
                IFlag flag = dimRegion.getFlag(regionFlag.name);
                // TODO: Check state with allowed
                flagCheck.setDeniedInDim(flag.isActive());
            } else {
                flagCheck.setDeniedInDim(false);
            }
        } else {
            flagCheck.setDeniedInDim(false);
        }

        if (flagCheck.getLocalRegion() == null) {
            flagCheck.setDenied(flagCheck.isDeniedInDim());
            return flagCheck;
        } else {
            boolean deniedResult = flagCheck.isDeniedInDim() && flagCheck.isDeniedLocal() || (!flagCheck.isDeniedInDim() || flagCheck.isDeniedLocal()) && (!flagCheck.isDeniedInDim() && (flagCheck.isDeniedLocal()) || !flagCheck.isDeniedInDim() && !flagCheck.isDeniedLocal());
            flagCheck.setDenied(deniedResult);
            return flagCheck;
        }
    }

    public static boolean checkTargetEventFor(BlockPos target, RegionFlag regionFlag, DimensionalRegion dimRegion) {
        IMarkableRegion involvedRegion = getInvolvedRegionFor(regionFlag, target, dimRegion.getDim());
        FlagCheckEvent flagCheck = new FlagCheckEvent(dimRegion, involvedRegion, regionFlag);
        if (involvedRegion == null) {
            flagCheck.setDeniedLocal(false);
        } else {
            IFlag flag = involvedRegion.getFlag(regionFlag.name);
            // TODO: Check state with allowed
            flagCheck.setDeniedLocal(flag.isActive());
        }
        if (dimRegion.isActive()) {
            if (dimRegion.containsFlag(regionFlag)) {
                IFlag flag = dimRegion.getFlag(regionFlag.name);
                // TODO: Check state with allowed
                flagCheck.setDeniedInDim(flag.isActive());
            } else {
                flagCheck.setDeniedInDim(false);
            }
        } else {
            flagCheck.setDeniedInDim(false);
        }

        if (flagCheck.getLocalRegion() == null) {
            flagCheck.setDenied(flagCheck.isDeniedInDim());
        } else {
            boolean deniedResult = flagCheck.isDeniedInDim() && flagCheck.isDeniedLocal() || (!flagCheck.isDeniedInDim() || flagCheck.isDeniedLocal()) && (!flagCheck.isDeniedInDim() && (flagCheck.isDeniedLocal()) || !flagCheck.isDeniedInDim() && !flagCheck.isDeniedLocal());
            flagCheck.setDenied(deniedResult);
        }
        return flagCheck.isDenied();
    }
}
