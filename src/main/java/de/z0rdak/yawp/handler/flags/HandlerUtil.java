package de.z0rdak.yawp.handler.flags;

import de.z0rdak.yawp.core.flag.IFlag;
import de.z0rdak.yawp.core.flag.RegionFlag;
import de.z0rdak.yawp.core.region.DimensionalRegion;
import de.z0rdak.yawp.core.region.IMarkableRegion;
import de.z0rdak.yawp.managers.data.region.DimensionRegionCache;
import de.z0rdak.yawp.managers.data.region.RegionDataManager;
import de.z0rdak.yawp.util.RegionUtil;
import net.minecraft.entity.Entity;
import net.minecraft.entity.FlyingEntity;
import net.minecraft.entity.boss.dragon.EnderDragonEntity;
import net.minecraft.entity.monster.MonsterEntity;
import net.minecraft.entity.monster.ShulkerEntity;
import net.minecraft.entity.monster.SlimeEntity;
import net.minecraft.entity.passive.AnimalEntity;
import net.minecraft.entity.passive.WaterMobEntity;
import net.minecraft.entity.player.PlayerEntity;
import net.minecraft.util.RegistryKey;
import net.minecraft.util.math.BlockPos;
import net.minecraft.world.World;
import net.minecraftforge.event.entity.EntityEvent;
import net.minecraftforge.event.world.ExplosionEvent;

import java.util.List;
import java.util.stream.Collectors;

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

    /**
     * Utility to check if a event is server-side.
     * @param event entity event to check side-ness for
     * @return true if entity event side-ness is server
     */
    public static boolean isServerSide(EntityEvent event){
        return isServerSide(event.getEntity());
    }

    public static boolean isServerSide(Entity entity){
        return !entity.getCommandSenderWorld().isClientSide();
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

    /**
     * Checks is any region contains the specified flag
     * @param regions regions to check for
     * @param flag flag to be checked for
     * @return true if any region contains the specified flag, false otherwise
     */
    public static boolean anyRegionContainsFlag(List<IMarkableRegion> regions, IFlag flag){
        return regions.stream()
                .anyMatch(region -> region.containsFlag(flag));
    }

    /**
     * Filters affected blocks from explosion event which are in a region with the specified flag.
     * @param event detonation event
     * @param flag flag to be filtered for
     * @return list of block positions which are in a region with the specified flag
     */
    public static List<BlockPos> filterExplosionAffectedBlocks(ExplosionEvent.Detonate event, IFlag flag){
        return event.getAffectedBlocks().stream()
                .filter(blockPos -> anyRegionContainsFlag(
                        RegionUtil.getHandlingRegionsFor(blockPos, event.getWorld()), flag))
                .collect(Collectors.toList());
    }

    public static List<Entity> filterAffectedEntities(ExplosionEvent.Detonate event, IFlag flag) {
        return event.getAffectedEntities().stream()
                .filter(entity -> anyRegionContainsFlag(
                        RegionUtil.getHandlingRegionsFor(entity.blockPosition(), event.getWorld()), flag))
                .collect(Collectors.toList());
    }
}
