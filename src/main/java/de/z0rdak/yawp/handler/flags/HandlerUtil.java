package de.z0rdak.yawp.handler.flags;

import de.z0rdak.yawp.core.flag.RegionFlag;
import de.z0rdak.yawp.core.region.DimensionalRegion;
import de.z0rdak.yawp.managers.data.region.DimensionRegionCache;
import de.z0rdak.yawp.managers.data.region.RegionDataManager;
import net.minecraft.entity.Entity;
import net.minecraft.entity.FlyingEntity;
import net.minecraft.entity.boss.dragon.EnderDragonEntity;
import net.minecraft.entity.monster.MonsterEntity;
import net.minecraft.entity.monster.ShulkerEntity;
import net.minecraft.entity.monster.SlimeEntity;
import net.minecraft.entity.passive.AnimalEntity;
import net.minecraft.entity.passive.WaterMobEntity;
import net.minecraft.entity.player.PlayerEntity;
import net.minecraft.util.Hand;
import net.minecraft.util.RegistryKey;
import net.minecraft.world.World;

public final class HandlerUtil {

    private HandlerUtil(){}

    public static RegistryKey<World> getEntityDim(Entity entity){
        return entity.getCommandSenderWorld().dimension();
    }

    public static DimensionalRegion getResponsibleDimRegion(Entity entity){
        return RegionDataManager.get().cacheFor(entity.getCommandSenderWorld().dimension()).getDimensionalRegion();
    }

    public static boolean isAnimal(Entity entity){
        return entity instanceof AnimalEntity || entity instanceof WaterMobEntity;
    }

    public static boolean isMonster(Entity entity){
        return entity instanceof MonsterEntity
                || entity instanceof SlimeEntity
                || entity instanceof FlyingEntity
                || entity instanceof EnderDragonEntity
                || entity instanceof ShulkerEntity;
    }

    public static boolean hasNoAffiliationFor(DimensionRegionCache dimCache, PlayerEntity player){
        return !(dimCache.hasMember(player) || dimCache.hasOwner(player));
    }

    public static boolean containsFlagAndHasNoAffiliationFor(DimensionRegionCache dimCache, RegionFlag flag, PlayerEntity player){
        return dimCache.getDimensionalRegion().containsFlag(flag) && hasNoAffiliationFor(dimCache, player);
    }
}
