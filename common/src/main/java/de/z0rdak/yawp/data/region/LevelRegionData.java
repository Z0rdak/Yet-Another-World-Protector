package de.z0rdak.yawp.data.region;

import com.mojang.serialization.Codec;
import com.mojang.serialization.codecs.RecordCodecBuilder;
import de.z0rdak.yawp.constants.Constants;
import de.z0rdak.yawp.core.region.DimensionalRegion;
import de.z0rdak.yawp.core.region.IMarkableRegion;
import de.z0rdak.yawp.core.region.MarkedRegion;
import net.minecraft.core.registries.Registries;
import net.minecraft.resources.ResourceKey;
import net.minecraft.resources.ResourceLocation;
import net.minecraft.world.level.Level;
import net.minecraft.world.level.saveddata.SavedData;
import net.minecraft.world.level.saveddata.SavedDataType;

import java.util.HashMap;
import java.util.Map;

public class LevelRegionData extends SavedData {

    private final ResourceLocation id;
    private Map<String, IMarkableRegion> locals;
    private DimensionalRegion dim;

    public static Codec<LevelRegionData> CODEC = RecordCodecBuilder.create(
            instance -> instance.group(
                    ResourceLocation.CODEC.fieldOf("id")
                            .forGetter(lrd -> lrd.id),
                    DimensionalRegion.CODEC.fieldOf("dim_region")
                            .forGetter(lrd -> lrd.dim),
                    Codec.unboundedMap(Codec.STRING, MarkedRegion.CODEC).fieldOf("local_regions")
                            .forGetter(lrd -> lrd.locals)
            ).apply(instance, LevelRegionData::new));

    public static SavedDataType<LevelRegionData> buildSavedDataType(ResourceLocation dimRl) {
        var dimId = dimRl.toString().replace(ResourceLocation.NAMESPACE_SEPARATOR, '_');
        return new SavedDataType<>(
                String.join("/", Constants.MOD_ID, dimId),
                (ctx) -> new LevelRegionData(dimRl),
                (ctx) -> CODEC,
                null);
    }

    public LevelRegionData(ResourceLocation id, DimensionalRegion dim, Map<String, IMarkableRegion> locals) {
        this(id);
        this.dim = dim;
        this.locals = locals;
    }

    public LevelRegionData(ResourceLocation id) {
        this.id = id;
        this.locals = new HashMap<>();
        var global = RegionDataManager.get().getGlobalRegion();
        ResourceKey<Level> levelRk = ResourceKey.create(Registries.DIMENSION, id);
        this.dim = new DimensionalRegion(levelRk, global);
    }
}

