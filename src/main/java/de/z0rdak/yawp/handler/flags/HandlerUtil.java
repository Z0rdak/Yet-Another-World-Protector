package de.z0rdak.yawp.handler.flags;

import de.z0rdak.yawp.core.flag.IFlag;
import de.z0rdak.yawp.core.flag.RegionFlag;
import de.z0rdak.yawp.core.region.DimensionalRegion;
import de.z0rdak.yawp.core.region.IMarkableRegion;
import de.z0rdak.yawp.util.MessageUtil;
import net.minecraft.entity.Entity;
import net.minecraft.entity.boss.dragon.EnderDragonEntity;
import net.minecraft.entity.mob.*;
import net.minecraft.entity.passive.AnimalEntity;
import net.minecraft.entity.passive.MerchantEntity;
import net.minecraft.entity.player.PlayerEntity;
import net.minecraft.registry.RegistryKey;
import net.minecraft.util.math.BlockPos;
import net.minecraft.world.World;

import static de.z0rdak.yawp.util.LocalRegions.getInvolvedRegionFor;

public final class HandlerUtil {

    private HandlerUtil() {
    }

    public static RegistryKey<World> getEntityDim(Entity entity) {
        return entity.getWorld().getRegistryKey();
    }

    public static boolean isAnimal(Entity entity) {
        return entity instanceof AnimalEntity || entity instanceof WaterCreatureEntity;
    }

    public static boolean isServerSide(Entity entity) {
        return !entity.getWorld().isClient;
    }

    public static boolean isServerSide(World world) {
        return !world.isClient;
    }

    public static boolean isVillager(Entity entity) {
        return entity instanceof MerchantEntity;
    }

    public static boolean isPlayer(Entity entity) {
        return entity instanceof PlayerEntity;
    }

    public static boolean isMonster(Entity entity) {
        return entity instanceof HostileEntity
                || entity instanceof SlimeEntity
                || entity instanceof FlyingEntity
                || entity instanceof EnderDragonEntity
                || entity instanceof ShulkerEntity;
    }

    public static boolean handleAndSendMsg(FlagCheckEvent.PlayerFlagEvent flagCheck) {
        if (flagCheck.getLocalRegion() == null && flagCheck.isDeniedInDim()) {
            MessageUtil.sendDimFlagNotification(flagCheck.getPlayer(), flagCheck.getFlag());
        }
        if (flagCheck.isDeniedLocal()) {
            if (!flagCheck.getLocalRegion().isMuted()) {
                MessageUtil.sendFlagNotification(flagCheck.getPlayer(), flagCheck.getLocalRegion(), flagCheck.getFlag());
            }
        }
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

    public static boolean sendFlagDeniedMsg(FlagCheckEvent flagCheck, PlayerEntity player) {
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

    public static FlagCheckEvent.PlayerFlagEvent checkPlayerEvent(PlayerEntity player, BlockPos target, RegionFlag regionFlag, DimensionalRegion dimRegion) {
        IMarkableRegion involvedRegion = getInvolvedRegionFor(regionFlag, target, player, player.world.getRegistryKey());
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
