package de.z0rdak.yawp.data.region;

import com.mojang.serialization.Codec;
import com.mojang.serialization.codecs.RecordCodecBuilder;
import de.z0rdak.yawp.constants.Constants;
import net.minecraft.resources.Identifier;
import net.minecraft.server.level.ServerLevel;
import net.minecraft.world.level.saveddata.SavedData;
import net.minecraft.world.level.saveddata.SavedDataType;

import java.util.*;

import static net.minecraft.world.level.Level.*;

public class LevelListData extends SavedData {

    public static final String DIMENSIONS_FILE_NAME = "dimensions";

    public static Codec<LevelListData> LEVEL_LIST_CODEC = RecordCodecBuilder.create(
            instance -> instance.group(
                    Codec.list(Identifier.CODEC).optionalFieldOf("dims", new ArrayList<>())
                            .forGetter(LevelListData::getLevels)
            ).apply(instance, LevelListData::new));
    public final static SavedDataType<LevelListData> TYPE = new SavedDataType<>(
            Identifier.fromNamespaceAndPath(Constants.MOD_ID, DIMENSIONS_FILE_NAME),
            LevelListData::new,
            LEVEL_LIST_CODEC,
            null);

    private final Set<Identifier> dimensions;

    public LevelListData(List<Identifier> dims){
        this.dimensions = new HashSet<>(dims);
    }

    public LevelListData(){
        this.dimensions = new HashSet<>();
    }

    @Override
    public @NotNull CompoundTag save(@NotNull CompoundTag tag, HolderLookup.Provider provider) {
        Optional<Tag> nbt = LevelListData.CODEC.encodeStart(NbtOps.INSTANCE, this)
                .resultOrPartial(Constants.LOGGER::warn);
        if (nbt.isPresent()) {
            tag = (CompoundTag) nbt.get();
        }
        return tag;
    }

    public boolean doesTrack(Identifier rl) {
        return this.hasDimEntry(rl);
    }

    public boolean doesTrack(ServerLevel level) {
        return this.doesTrack(level.dimension().location());
    }

    public static LevelListData get(DimensionDataStorage storage, @Nullable Supplier<LevelListData> defaultSupplier) {
        Supplier<LevelListData> supplier = defaultSupplier == null ? LevelListData::new : defaultSupplier;
        var factory = new Factory<>(supplier, LevelListData::load, DataFixTypes.SAVED_DATA_MAP_DATA);
        return storage.computeIfAbsent(factory, LevelListData.TYPE);
    }

    public static LevelListData load(CompoundTag tag, HolderLookup.Provider provider) {
        return LevelListData.CODEC.parse(NbtOps.INSTANCE, tag)
                .resultOrPartial(Constants.LOGGER::warn)
                .orElse(new LevelListData());
    }

    public List<Identifier> getLevels() {
        return new ArrayList<>(this.dimensions);
    }

    public void addTrackingFor(Identifier rl) {
        this.dimensions.add(rl);
    }

    public void removeTrackingFor(Identifier rl) {
        this.dimensions.remove(rl);
    }

    private boolean hasDimEntry(Identifier rl) {
        return this.dimensions.contains(rl);
    }
}

