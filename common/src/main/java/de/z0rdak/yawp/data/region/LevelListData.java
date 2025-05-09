package de.z0rdak.yawp.data.region;

import com.mojang.serialization.Codec;
import com.mojang.serialization.codecs.RecordCodecBuilder;
import de.z0rdak.yawp.constants.Constants;
import net.minecraft.resources.ResourceLocation;
import net.minecraft.world.level.saveddata.SavedData;
import net.minecraft.world.level.saveddata.SavedDataType;

import java.util.*;

public class LevelListData extends SavedData {

    public static final String DIMENSIONS_FILE_NAME = "dimensions";

    public static Codec<LevelListData> LEVEL_LIST_CODEC = RecordCodecBuilder.create(
            instance -> instance.group(
                    Codec.list(ResourceLocation.CODEC).optionalFieldOf("dims", new ArrayList<>())
                            .forGetter(LevelListData::getLevels)
            ).apply(instance, LevelListData::new));
    public final static SavedDataType<LevelListData> TYPE = new SavedDataType<>(
            String.join("/", Constants.MOD_ID, DIMENSIONS_FILE_NAME),
            (ctx) -> new LevelListData(),
            (ctx) -> LEVEL_LIST_CODEC,
            null);

    private final Set<ResourceLocation> dimensions;

    public LevelListData(List<ResourceLocation> dims){
        this.dimensions = new HashSet<>(dims);
    }

    public LevelListData(){
        this.dimensions = new HashSet<>();
    }

    public List<ResourceLocation> getLevels() {
        return new ArrayList<>(this.dimensions);
    }

    public void addDimEntry(ResourceLocation rl) {
        this.dimensions.add(rl);
    }

    public boolean hasDimEntry(ResourceLocation rl) {
        return this.dimensions.contains(rl);
    }
}

