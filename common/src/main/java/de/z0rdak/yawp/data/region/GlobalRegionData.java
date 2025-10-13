package de.z0rdak.yawp.data.region;

import com.mojang.serialization.Codec;
import com.mojang.serialization.codecs.RecordCodecBuilder;
import de.z0rdak.yawp.constants.Constants;
import de.z0rdak.yawp.core.region.GlobalRegion;
import net.minecraft.core.HolderLookup;
import net.minecraft.nbt.CompoundTag;
import net.minecraft.nbt.NbtOps;
import net.minecraft.nbt.Tag;
import net.minecraft.resources.ResourceLocation;
import net.minecraft.server.level.ServerLevel;
import net.minecraft.util.datafix.DataFixTypes;
import net.minecraft.world.level.saveddata.SavedData;
import net.minecraft.world.level.storage.DimensionDataStorage;
import org.jetbrains.annotations.NotNull;
import org.jetbrains.annotations.Nullable;

import java.util.Optional;
import java.util.function.Supplier;

public class GlobalRegionData extends SavedData {

    public static final String GLOBAL_REGION_FILE_NAME = "global";
    public static final String TYPE = Constants.MOD_ID + "/" + GLOBAL_REGION_FILE_NAME;

    public static final Codec<GlobalRegionData> CODEC = RecordCodecBuilder.create(
            instance -> instance.group(
                            ResourceLocation.CODEC.fieldOf("id")
                                    .forGetter(r -> GlobalRegion.GLOBAL),
                            GlobalRegion.CODEC.optionalFieldOf("global", new GlobalRegion())
                                    .forGetter(GlobalRegionData::getGlobal))
                    .apply(instance, GlobalRegionData::new));

    private final ResourceLocation id;
    private GlobalRegion globalRegion;

    public GlobalRegionData() {
        this.id = GlobalRegion.GLOBAL;
        this.globalRegion = new GlobalRegion();
    }

    public GlobalRegionData(ResourceLocation id, GlobalRegion region) {
        this.id = id;
        this.globalRegion = region;
    }

    public static GlobalRegionData load(CompoundTag tag, HolderLookup.Provider provider) {
        return GlobalRegionData.CODEC.parse(NbtOps.INSTANCE, tag)
                .resultOrPartial(Constants.LOGGER::warn)
                .orElse(new GlobalRegionData());
    }

    @Override
    public @NotNull CompoundTag save(@NotNull CompoundTag tag, HolderLookup.Provider provider) {
        Optional<Tag> nbt = GlobalRegionData.CODEC.encodeStart(NbtOps.INSTANCE, this)
                .resultOrPartial(Constants.LOGGER::warn);
        if (nbt.isPresent()) {
            tag = (CompoundTag) nbt.get();
        }
        return tag;
    }

    public GlobalRegion getGlobal() {
        return globalRegion;
    }

    public void reset() {
        this.globalRegion = new GlobalRegion();
    }

    public static GlobalRegionData get(DimensionDataStorage dataStorage, @Nullable Supplier<GlobalRegionData> globalRegionDataSupplier) {
        Supplier<GlobalRegionData> supplier = globalRegionDataSupplier != null ? globalRegionDataSupplier : GlobalRegionData::new;
        var factory = new SavedData.Factory<GlobalRegionData>(supplier, GlobalRegionData::load, DataFixTypes.SAVED_DATA_MAP_DATA);
        return dataStorage.computeIfAbsent(factory, GlobalRegionData.TYPE);
    }
}

