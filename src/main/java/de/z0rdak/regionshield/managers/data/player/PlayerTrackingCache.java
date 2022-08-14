package de.z0rdak.regionshield.managers.data.player;

import de.z0rdak.regionshield.core.region.AbstractMarkableRegion;
import de.z0rdak.regionshield.core.region.IMarkableRegion;
import net.minecraft.entity.player.PlayerEntity;
import net.minecraft.nbt.CompoundNBT;
import net.minecraft.util.RegistryKey;
import net.minecraft.util.math.ChunkPos;
import net.minecraft.util.math.SectionPos;
import net.minecraft.world.World;
import net.minecraftforge.common.util.INBTSerializable;
import net.minecraftforge.common.world.ForgeChunkManager;

import java.util.HashMap;
import java.util.List;
import java.util.Map;

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
public class PlayerTrackingCache extends HashMap<PlayerEntity, List<IMarkableRegion>> implements INBTSerializable<CompoundNBT> {

    public final RegistryKey<World> dimKey;

    public PlayerTrackingCache(RegistryKey<World> dim){
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
    public CompoundNBT serializeNBT() {
        return null;
    }

    @Override
    public void deserializeNBT(CompoundNBT nbt) {

    }
}
