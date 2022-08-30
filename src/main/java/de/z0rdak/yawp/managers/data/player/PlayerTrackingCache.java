package de.z0rdak.yawp.managers.data.player;

import de.z0rdak.yawp.core.region.IMarkableRegion;
import net.minecraft.nbt.CompoundTag;
import net.minecraft.resources.ResourceKey;
import net.minecraft.world.entity.player.Player;
import net.minecraft.world.level.Level;
import net.minecraftforge.common.util.INBTSerializable;

import java.util.HashMap;
import java.util.List;

/*
Unrelated:
 - TODO: prevent teleporting to into a region with conflicting permissions
Related:
 - TODO: Idea save regions separately for players, so that cache can be cleared when players are not loaded and saved to dßisk
 - TODO: Only consider regions in a chunk radius 3x3 around a player
 */
// TODO: refresh positions on login and logout

/**
 * Cache for player tracking near regions to handle enter and leaving events
 */
public class PlayerTrackingCache extends HashMap<Player, List<IMarkableRegion>> implements INBTSerializable<CompoundTag> {

    public final ResourceKey<Level> dimKey;

    public PlayerTrackingCache(ResourceKey<Level> dim){
        this.dimKey = dim;
    }
    
    /*
    // UUID
    // Region -> List<Player>
    // Region -> List<Chunk> ?
    // Chunk
    // Chunk -> List<Player>
    // Chunk -> List<Region>
    // SectionPos -> List<Player>

    //ForgeChunkManager. ?

    //private Map<ChunkPos, List<IMarkableRegion>> regionsOffLoadedChunks = new HashMap<>();

*/

    @Override
    public CompoundTag serializeNBT() {
        return null;
    }

    @Override
    public void deserializeNBT(CompoundTag nbt) {

    }
}
