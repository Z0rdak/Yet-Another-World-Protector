package de.z0rdak.yawp.data.region;

import com.mojang.serialization.Codec;
import com.mojang.serialization.codecs.RecordCodecBuilder;
import de.z0rdak.yawp.constants.Constants;
import net.minecraft.nbt.CompoundTag;
import net.minecraft.nbt.NbtOps;
import net.minecraft.nbt.Tag;
import net.minecraft.resources.ResourceLocation;
import net.minecraft.server.level.ServerLevel;
import net.minecraft.world.level.saveddata.SavedData;
import net.minecraft.world.level.storage.DimensionDataStorage;
import org.jetbrains.annotations.NotNull;
import org.jetbrains.annotations.Nullable;

import java.util.*;
import java.util.function.Supplier;

import static net.minecraft.world.level.Level.*;

public class LevelListData extends SavedData {

    public static final String DIMENSIONS_FILE_NAME = "dimensions";

    public static Codec<LevelListData> CODEC = RecordCodecBuilder.create(
            instance -> instance.group(
                    Codec.list(ResourceLocation.CODEC).optionalFieldOf("dims", new ArrayList<>())
                            .forGetter(LevelListData::getLevels)
            ).apply(instance, LevelListData::new));

    public final static String TYPE = String.join("/", Constants.MOD_ID, DIMENSIONS_FILE_NAME);

    private final Set<ResourceLocation> dimensions;

    public LevelListData(List<ResourceLocation> dims){
        this.dimensions = new HashSet<>(dims);
    }

    public LevelListData(){
        this.dimensions = new HashSet<>();
    }

    @Override
    public @NotNull CompoundTag save(@NotNull CompoundTag tag) {
        Optional<Tag> nbt = LevelListData.CODEC.encodeStart(NbtOps.INSTANCE, this)
                .resultOrPartial(Constants.LOGGER::warn);
        if (nbt.isPresent()) {
            tag = (CompoundTag) nbt.get();
        }
        return tag;
    }

    public boolean doesTrack(ResourceLocation rl) {
        return this.hasDimEntry(rl);
    }

    public boolean doesTrack(ServerLevel level) {
        return this.doesTrack(level.dimension().location());
    }

    public static LevelListData get(DimensionDataStorage storage, @Nullable Supplier<LevelListData> defaultSupplier) {
        return storage.computeIfAbsent(
                LevelListData::load,
                defaultSupplier == null ? LevelListData::new : defaultSupplier,
                LevelListData.TYPE);
    }

    public static LevelListData load(CompoundTag tag) {
        return LevelListData.CODEC.parse(NbtOps.INSTANCE, tag)
                .resultOrPartial(Constants.LOGGER::warn)
                .orElse(new LevelListData());
    }

    public List<ResourceLocation> getLevels() {
        return new ArrayList<>(this.dimensions);
    }

    public void addTrackingFor(ResourceLocation rl) {
        this.dimensions.add(rl);
    }

    public void removeTrackingFor(ResourceLocation rl) {
        this.dimensions.remove(rl);
    }

    private boolean hasDimEntry(ResourceLocation rl) {
        return this.dimensions.contains(rl);
    }
}

