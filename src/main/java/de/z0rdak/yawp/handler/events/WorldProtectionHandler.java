package de.z0rdak.yawp.handler.events;

import de.z0rdak.yawp.YetAnotherWorldProtector;
import de.z0rdak.yawp.config.server.FlagConfig;
import de.z0rdak.yawp.core.flag.RegionFlag;
import de.z0rdak.yawp.core.region.DimensionalRegion;
import de.z0rdak.yawp.managers.data.region.DimensionRegionCache;
import de.z0rdak.yawp.managers.data.region.RegionDataManager;
import net.minecraft.entity.Entity;
import net.minecraft.entity.player.PlayerEntity;
import net.minecraft.util.RegistryKey;
import net.minecraft.util.ResourceLocation;
import net.minecraft.world.World;
import net.minecraftforge.event.entity.player.AttackEntityEvent;
import net.minecraftforge.event.world.BlockEvent;
import net.minecraftforge.eventbus.api.EventPriority;
import net.minecraftforge.eventbus.api.SubscribeEvent;
import net.minecraftforge.fml.common.Mod;

import java.util.List;

@Mod.EventBusSubscriber(modid = YetAnotherWorldProtector.MODID)
public final class WorldProtectionHandler {

    private WorldProtectionHandler(){}

    @SubscribeEvent
    public static void onBlockBreak(BlockEvent.BreakEvent breakEvent) {
        if(!breakEvent.getWorld().isClientSide()) {
            RegistryKey<World> dim =  breakEvent.getPlayer().getCommandSenderWorld().dimension();
            DimensionRegionCache dimCache =  RegionDataManager.get().cacheFor(dim);
            DimensionalRegion dimRegion = dimCache.getDimensionalRegion();
            if (dimRegion.getFlags().contains(RegionFlag.BREAK_BLOCKS.flag)) {
                breakEvent.setCanceled(true);
                return;
            }
        }
    }

    @SubscribeEvent(priority = EventPriority.HIGHEST)
    public static void DimOnEntityBreak(AttackEntityEvent attackEntityEvent) {
        if(!attackEntityEvent.getPlayer().getEntity().getCommandSenderWorld().isClientSide()) {
            Entity target = attackEntityEvent.getTarget();
            List<? extends String> entities = FlagConfig.BREAK_FLAG_ENTITIES.get();
            entities.forEach( entity -> {
                ResourceLocation entityResourceLocation = new ResourceLocation(entity);
                if (target.getType().getRegistryName() != null
                        && target.getType().getRegistryName().equals(entityResourceLocation)){
                    attackEntityEvent.setCanceled(true);
                    return;
                }
            });
        }
    }

    @SubscribeEvent(priority = EventPriority.HIGH, receiveCanceled = true)
    public static void onEntityBreak(AttackEntityEvent attackEntityEvent) {
        if (!attackEntityEvent.getPlayer().getEntity().getCommandSenderWorld().isClientSide()) {
            return;
        }
    }

    // TODO: remove test
    @SubscribeEvent
    public static void onBlockPlace(BlockEvent.EntityPlaceEvent placeEvent) {
        if(!placeEvent.getWorld().isClientSide()) {
            if (placeEvent.getEntity() instanceof PlayerEntity) {
                PlayerEntity player = (PlayerEntity) placeEvent.getEntity();
                // can be used for fire placement with flint
                RegistryKey<World> dim =  player.getCommandSenderWorld().dimension();
                DimensionRegionCache dimCache =  RegionDataManager.get().cacheFor(dim);
                DimensionalRegion dimRegion = dimCache.getDimensionalRegion();

            }

        }
    }
}
