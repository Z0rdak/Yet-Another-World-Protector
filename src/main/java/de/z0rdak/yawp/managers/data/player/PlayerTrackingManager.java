package de.z0rdak.yawp.managers.data.player;

import de.z0rdak.yawp.YetAnotherWorldProtector;
import net.minecraft.resources.ResourceKey;
import net.minecraft.server.level.ServerLevel;
import net.minecraft.world.entity.player.Player;
import net.minecraft.world.level.Level;
import net.minecraftforge.event.entity.EntityEvent;
import net.minecraftforge.event.level.ChunkEvent;
import net.minecraftforge.event.server.ServerStartingEvent;
import net.minecraftforge.eventbus.api.SubscribeEvent;
import org.apache.commons.lang3.NotImplementedException;

import java.util.HashMap;
import java.util.List;
import java.util.Map;

/**
 * Data manger to save player tracking data for players entering and leaving regions
 */
//@EventBusSubscriber(modid = YetAnotherWorldProtector.MODID, value = Dist.DEDICATED_SERVER)
public class PlayerTrackingManager {

    public final static Map<ResourceKey<Level>, PlayerTrackingCache> playerTrackingPerDim  = new HashMap<>(3); // amount of dims

    private static final PlayerTrackingManager playerTrackingManager = new PlayerTrackingManager();

    private PlayerTrackingManager(){
        playerTrackingPerDim.put(Level.OVERWORLD, new PlayerTrackingCache(Level.OVERWORLD));
        playerTrackingPerDim.put(Level.NETHER, new PlayerTrackingCache(Level.NETHER));
        playerTrackingPerDim.put(Level.END, new PlayerTrackingCache(Level.END));
    }

    public static PlayerTrackingManager get(){
        return playerTrackingManager;
    }

    public PlayerTrackingCache trackingCacheFor(ResourceKey<Level> dim){
        return playerTrackingPerDim.get(dim);
    }

    /*
    Keep track of the chunk the player is in and the regions in and surround this chunk to
    keep time for lookup small

     */



    @SubscribeEvent
    public static void onChunkLoad(ChunkEvent.Load event) {
        if (!event.getLevel().isClientSide()) {
            if (event.getChunk().getWorldForge() instanceof ServerLevel) {
                ServerLevel world = (ServerLevel) event.getChunk().getWorldForge();
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

    @SubscribeEvent
    public static void onChunkUnload(ChunkEvent.Unload event) {

    }

    @SubscribeEvent
    public static void onChunkChange(EntityEvent.EnteringSection event) {
        if (!event.getEntity().getCommandSenderWorld().isClientSide) {

            if (event.getEntity() instanceof Player) {
                Player player = (Player) event.getEntity();
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

    public static List<Player> getPlayersInRegionVicinity() {
        throw new NotImplementedException("getPlayersInRegionVicinity");
    }

    @SubscribeEvent
    public static void loadPlayerData(ServerStartingEvent event) {

    }
}
