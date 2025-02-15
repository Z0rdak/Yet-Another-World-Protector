package de.z0rdak.yawp.handler;

import de.z0rdak.yawp.api.permission.Permissions;
import de.z0rdak.yawp.core.flag.*;
import de.z0rdak.yawp.core.flag.FlagCorrelation;
import de.z0rdak.yawp.core.region.IProtectedRegion;
import net.minecraft.core.BlockPos;
import net.minecraft.resources.ResourceKey;
import net.minecraft.world.entity.Entity;
import net.minecraft.world.entity.FlyingMob;
import net.minecraft.world.entity.animal.Animal;
import net.minecraft.world.entity.animal.WaterAnimal;
import net.minecraft.world.entity.animal.horse.SkeletonHorse;
import net.minecraft.world.entity.animal.horse.ZombieHorse;
import net.minecraft.world.entity.boss.enderdragon.EnderDragon;
import net.minecraft.world.entity.monster.Enemy;
import net.minecraft.world.entity.monster.Shulker;
import net.minecraft.world.entity.monster.Slime;
import net.minecraft.world.entity.player.Player;
import net.minecraft.world.item.trading.Merchant;
import net.minecraft.world.level.Level;
import net.minecraft.world.level.LevelAccessor;
import org.jetbrains.annotations.Nullable;

import java.util.*;
import java.util.stream.Collectors;

public final class HandlerUtil {

    private HandlerUtil() {
    }

    public static ResourceKey<Level> getDimKey(Entity entity) {
        return entity.level().dimension();
    }

    public static ResourceKey<Level> getDimKey(Level world) {
        return world.dimension();
    }

    public static boolean isAnimal(Entity entity) {
        boolean isMonsterHorse = entity instanceof ZombieHorse || entity instanceof SkeletonHorse;
        // Note: because monster horses are still horses and horses are animals? what the frick mojang
        return entity instanceof Animal && !isMonsterHorse 
                || entity instanceof WaterAnimal;
    }

    public static boolean isServerSide(Entity entity) {
        return isServerSide(entity.level());
    }

    public static boolean isServerSide(LevelAccessor world) {
        return !world.isClientSide();
    }
    public static boolean isServerSide(Level world) {
        return !world.isClientSide;
    }

    public static boolean isVillager(Entity entity) {
        return entity instanceof Merchant;
    }

    public static boolean isPlayer(Entity entity) {
        return entity instanceof Player;
    }

    public static boolean isMonster(Entity entity) {
        return entity instanceof Enemy
                || entity instanceof Slime
                || entity instanceof FlyingMob
                || entity instanceof EnderDragon
                || entity instanceof Shulker 
                || entity instanceof ZombieHorse || entity instanceof SkeletonHorse;
    }

    public static boolean notServerSideOrPlayerNull(Entity entity) {
        return entity == null || !isServerSide(entity);
    }

    public static void syncPlayerInventory(Level world, Player player) {
        // TODO:
    }

    public static void updateBlockState(Level world, BlockPos pos) {
        world.updateNeighborsAt(pos, world.getBlockState(pos).getBlock());
    }


    public static Map<String, FlagCorrelation> getFlagMapRecursive(IProtectedRegion region, Map<String, FlagCorrelation> carry) {
        if (carry == null) {
            carry = region.getFlags().flagEntries().stream()
                    .filter(flag -> flag.getValue().getState() != FlagState.UNDEFINED)
                    .collect(Collectors.toMap(Map.Entry::getKey, entry -> new FlagCorrelation(region, entry.getValue())));
        }
        if (region.equals(region.getParent())) {
            // global region has itself as parent
            Set<Map.Entry<String, IFlag>> flags = getNonUndefinedFlags(region);
            for (Map.Entry<String, IFlag> entry : flags) {
                if (!carry.containsKey(entry.getKey())) {
                    carry.put(entry.getValue().getName(), new FlagCorrelation(region, entry.getValue()));
                }
            }
            return carry;
        }
        Set<Map.Entry<String, IFlag>> parentFlags = getNonUndefinedFlags(region.getParent());
        for (Map.Entry<String, IFlag> entry : parentFlags) {
            if (!carry.containsKey(entry.getKey())) {
                carry.put(entry.getValue().getName(), new FlagCorrelation(region.getParent(), entry.getValue()));
            }
            if (entry.getValue().doesOverride()) {
                carry.put(entry.getValue().getName(), new FlagCorrelation(region.getParent(), entry.getValue()));
            }
        }
        return getFlagMapRecursive(region.getParent(), carry);
    }

    private static Set<Map.Entry<String, IFlag>> getNonUndefinedFlags(IProtectedRegion region) {
        return region.getFlags().flagEntries().stream()
                .filter(flag -> flag.getValue().getState() != FlagState.UNDEFINED)
                .collect(Collectors.toSet());
    }

}
