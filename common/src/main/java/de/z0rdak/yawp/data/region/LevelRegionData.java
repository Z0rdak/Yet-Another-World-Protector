package de.z0rdak.yawp.data.region;

import com.mojang.serialization.Codec;
import com.mojang.serialization.Lifecycle;
import com.mojang.serialization.codecs.RecordCodecBuilder;
import de.z0rdak.yawp.api.commands.CommandConstants;
import de.z0rdak.yawp.commands.arguments.region.RegionArgumentType;
import de.z0rdak.yawp.constants.Constants;
import de.z0rdak.yawp.core.region.*;
import net.minecraft.core.registries.Registries;
import net.minecraft.resources.ResourceKey;
import net.minecraft.resources.ResourceLocation;
import net.minecraft.world.level.Level;
import net.minecraft.world.level.saveddata.SavedData;
import net.minecraft.world.level.saveddata.SavedDataType;

import java.util.*;
import java.util.stream.Collectors;

import static de.z0rdak.yawp.api.commands.CommandConstants.values;

public class LevelRegionData extends SavedData {

    public static Codec<LevelRegionData> CODEC = RecordCodecBuilder.create(
            instance -> instance.group(
                    ResourceLocation.CODEC.fieldOf("id")
                            .forGetter(lrd -> lrd.id),
                    DimensionalRegion.CODEC.fieldOf("dim_region")
                            .forGetter(lrd -> lrd.dim),
                    Codec.unboundedMap(Codec.STRING, MarkedRegion.CODEC).optionalFieldOf("local_regions", Lifecycle.stable(), new HashMap<>(), Lifecycle.stable())
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

    private final ResourceLocation id;
    private  HashMap<String, IMarkableRegion> locals;
    private DimensionalRegion dim;

    public LevelRegionData(ResourceLocation id, DimensionalRegion dim, Map<String, IMarkableRegion> locals) {
        this(id);
        this.dim = dim;
        this.locals.putAll(locals);
    }

    public LevelRegionData(ResourceLocation id, DimensionalRegion dim) {
        this(id);
        this.dim = dim;
        this.locals = new HashMap<>();
    }

    public LevelRegionData(ResourceLocation id) {
        this.id = id;
        this.locals = new HashMap<>();
        var global = RegionDataManager.getGlobalRegion();
        ResourceKey<Level> levelRk = ResourceKey.create(Registries.DIMENSION, id);
        this.dim = new DimensionalRegion(levelRk, global);
    }

    public HashMap<String, IMarkableRegion> getLocals() {
        return locals;
    }

    public ResourceLocation getId() {
        return id;
    }

    public ResourceKey<Level> getDimKey() {
        return ResourceKey.create(Registries.DIMENSION, this.getId());
    }

    public int regionCount() {
        return locals.size();
    }

    public Collection<IMarkableRegion> getLocalList() {
        return locals.values().stream().toList();
    }

    public Collection<String> getLocalNames() {
        return locals.keySet().stream().toList();
    }

    public DimensionalRegion getDim() {
        return dim;
    }

    public void renameLocal(IMarkableRegion region, String regionName) {
        if (this.locals.containsKey(regionName)) {
            throw new IllegalArgumentException("Region with name '" + regionName + "' already exists in dimension '" + this.dim.getName() + "'!");
        }
        IMarkableRegion currentRegion = this.locals.get(region.getName());
        IProtectedRegion parent = currentRegion.getParent();
        this.removeLocal(currentRegion);
        currentRegion.rename(regionName);
        this.addLocal(parent, currentRegion);
    }

    /**
     * Method to check if a region name is valid for a given dimension. <br>
     * A region name is valid if it matches the pattern and is not already used in the dimension.
     *
     * @param regionName the name of the region to be checked.
     * @return -1 if the region name is invalid, 0 if the region name is valid, 1 if the region name is already used in the dimension.
     */
    public int isValidRegionName(String regionName) {
        List<String> commandStrings = Arrays.stream(values()).map(CommandConstants::toString).collect(Collectors.toList());
        if (!regionName.matches(RegionArgumentType.VALID_NAME_PATTERN.pattern())
                || commandStrings.contains(regionName.toLowerCase())) {
            return -1;
        }
        if (this.locals.containsKey(regionName)) {
            return 1;
        }
        return 0;
    }

    public void addLocal(IProtectedRegion parent, IMarkableRegion child) {
        parent.addChild(child);
        this.locals.put(child.getName(), child);
    }

    public void addLocal(IMarkableRegion child) {
        this.dim.addChild(child);
        this.locals.put(child.getName(), child);
    }

    public void removeLocal(IMarkableRegion region) {
        if (this.hasLocal(region.getName())) {
            this.locals.remove(region.getName());
            if (region.getParent().getRegionType() == RegionType.DIMENSION) {
                region.getParent().removeChild(region);
            }
        }
    }

    public boolean hasLocal(String regionName) {
        return locals.containsKey(regionName);
    }

    public IMarkableRegion getLocal(String regionName) {
        return locals.get(regionName);
    }

    public void clearLocals() {
        this.locals.clear();
        this.dim.clearChildren();
    }


}

