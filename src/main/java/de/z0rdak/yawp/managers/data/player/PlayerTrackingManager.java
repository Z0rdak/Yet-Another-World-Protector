package de.z0rdak.yawp.managers.data.player;

import de.z0rdak.yawp.YetAnotherWorldProtector;
import net.minecraft.entity.player.PlayerEntity;
import net.minecraft.util.RegistryKey;
import net.minecraft.world.World;
import net.minecraft.world.server.ServerWorld;
import net.minecraftforge.event.entity.EntityEvent;
import net.minecraftforge.event.world.ChunkEvent;
import net.minecraftforge.eventbus.api.SubscribeEvent;
import net.minecraftforge.fml.common.Mod;
import net.minecraftforge.fml.event.server.FMLServerStartingEvent;
import org.apache.commons.lang3.NotImplementedException;

import java.util.HashMap;
import java.util.List;
import java.util.Map;

/**
 * Data manger to save player tracking data for players entering and leaving regions
 */
public class PlayerTrackingManager {

    public final static Map<RegistryKey<World>, PlayerTrackingCache> playerTrackingPerDim  = new HashMap<>(3); // amount of dims

    private static final PlayerTrackingManager playerTrackingManager = new PlayerTrackingManager();

    private PlayerTrackingManager(){
        playerTrackingPerDim.put(World.OVERWORLD, new PlayerTrackingCache(World.OVERWORLD));
        playerTrackingPerDim.put(World.NETHER, new PlayerTrackingCache(World.NETHER));
        playerTrackingPerDim.put(World.END, new PlayerTrackingCache(World.END));
    }

    public static PlayerTrackingManager get(){
        return playerTrackingManager;
    }

    public PlayerTrackingCache trackingCacheFor(RegistryKey<World> dim){
        return playerTrackingPerDim.get(dim);
    }

    /*
    Keep track of the chunk the player is in and the regions in and surround this chunk to
    keep time for lookup small

     */



    //@SubscribeEvent
    public static void onChunkLoad(ChunkEvent.Load event) {
        if (!event.getWorld().isClientSide()) {
            if (event.getChunk().getWorldForge() instanceof ServerWorld) {
                ServerWorld world = (ServerWorld) event.getChunk().getWorldForge();
                /*
                PlayerTrackingManager managerForDim = DimensionalRegionCache.regionChunkCachePerDim.get(world.dimension());
                if (!managerForDim.regionsOffLoadedChunks.containsKey(event.getChunk().getPos())) {
                    managerForDim.regionsOffLoadedChunks.put(event.getChunk().getPos(), new ArrayList<>());
                }
                // todo: get regions crossing chunk
                List<AbstractMarkableRegion> regions = new ArrayList<>();
                managerForDim.regionsOffLoadedChunks.put(event.getChunk().getPos(), regions);

                 */
            }
        }
    }

    //@SubscribeEvent
    public static void onChunkUnload(ChunkEvent.Unload event) {

    }

    //@SubscribeEvent
    public static void onChunkChange(EntityEvent.EnteringChunk event) {
        if (!event.getEntity().getCommandSenderWorld().isClientSide) {

            if (event.getEntity() instanceof PlayerEntity) {
                PlayerEntity player = (PlayerEntity) event.getEntity();
                // is player pos here the old or new chunkpos?
                // either way we don't have the new or old Y pos - how to fix this?
                // just ignore and 1.16 works only for x,z ?
/*
                try {
                    Method getChunksMethod = ChunkManager.class.getDeclaredMethod("getChunks");
                    ServerWorld world = (ServerWorld) event.getEntity().getCommandSenderWorld();
                    Iterable<ChunkHolder> loadedChunks = (Iterable<ChunkHolder>) getChunksMethod.invoke(world);
                    loadedChunks.forEach(chunk -> {

                    });
                } catch (NoSuchMethodException | InvocationTargetException | IllegalAccessException e) {
                    YetAnotherWorldProtector.LOGGER.error(e.getMessage());
                }

 */
            }
        }
    }

    public static List<PlayerEntity> getPlayersInRegionVicinity() {
        throw new NotImplementedException("getPlayersInRegionVicinity");
    }

    public static void loadPlayerData(FMLServerStartingEvent event) {

    }
}
