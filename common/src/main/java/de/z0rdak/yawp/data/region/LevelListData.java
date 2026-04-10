package de.z0rdak.yawp.data.region;

import com.mojang.serialization.Codec;
import com.mojang.serialization.codecs.RecordCodecBuilder;
import de.z0rdak.yawp.constants.Constants;
import net.minecraft.resources.Identifier;
import net.minecraft.server.level.ServerLevel;
import net.minecraft.world.level.saveddata.SavedData;
import net.minecraft.world.level.saveddata.SavedDataType;

import java.util.ArrayList;
import java.util.HashSet;
import java.util.List;
import java.util.Set;

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

    public boolean doesTrack(Identifier rl) {
        return this.hasDimEntry(rl);
    }

    public boolean doesTrack(ServerLevel level) {
        return this.doesTrack(level.dimension().identifier());
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

