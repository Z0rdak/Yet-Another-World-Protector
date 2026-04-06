package de.z0rdak.yawp.data.region;

import com.mojang.serialization.Codec;
import com.mojang.serialization.codecs.RecordCodecBuilder;
import de.z0rdak.yawp.constants.Constants;
import de.z0rdak.yawp.core.region.GlobalRegion;
import net.minecraft.resources.Identifier;
import net.minecraft.world.level.saveddata.SavedData;
import net.minecraft.world.level.saveddata.SavedDataType;

public class GlobalRegionData extends SavedData {

    public static final String GLOBAL_REGION_FILE_NAME = "global";

    public static final Codec<GlobalRegionData> CODEC = RecordCodecBuilder.create(
            instance -> instance.group(
                    Identifier.CODEC.fieldOf("id")
                            .forGetter(r -> GlobalRegion.GLOBAL),
                    GlobalRegion.CODEC.optionalFieldOf("global", new GlobalRegion())
                            .forGetter(GlobalRegionData::getGlobal))
                    .apply(instance, GlobalRegionData::new));

    public final static SavedDataType<GlobalRegionData> TYPE = new SavedDataType<GlobalRegionData>(
            Identifier.fromNamespaceAndPath(Constants.MOD_ID, GLOBAL_REGION_FILE_NAME),
            GlobalRegionData::new, CODEC,
            null);

    private final Identifier id;
    private GlobalRegion globalRegion;

    public GlobalRegionData(){
       this.id = GlobalRegion.GLOBAL;
       this.globalRegion = new GlobalRegion();
    }

    public GlobalRegionData(Identifier id, GlobalRegion globalRegion) {
        this.globalRegion = globalRegion;
        this.id = id;
    }

    public GlobalRegion getGlobal() {
        return globalRegion;
    }

    public Identifier getId() {
        return id;
    }

    public void reset() {
        this.globalRegion = new GlobalRegion();
    }
}

