package de.z0rdak.yawp.data.region;

import com.mojang.serialization.Codec;
import com.mojang.serialization.Lifecycle;
import com.mojang.serialization.codecs.RecordCodecBuilder;
import de.z0rdak.yawp.api.commands.CommandConstants;
import de.z0rdak.yawp.constants.Constants;
import de.z0rdak.yawp.core.region.*;
import net.minecraft.core.UUIDUtil;
import net.minecraft.core.registries.Registries;
import net.minecraft.resources.ResourceKey;
import net.minecraft.resources.Identifier;
import net.minecraft.world.level.Level;
import net.minecraft.world.level.saveddata.SavedData;
import net.minecraft.world.level.saveddata.SavedDataType;
import org.jetbrains.annotations.Nullable;

import java.util.*;
import java.util.stream.Collectors;

// TODO create API service

public class LevelData extends SavedData {

    public static Codec<LevelData> CODEC = RecordCodecBuilder.create(
            instance -> instance.group(
                    Identifier.CODEC.fieldOf("id")
                            .forGetter(lrd -> lrd.id),
                    DimensionalRegion.CODEC.fieldOf("dim_region")
                            .forGetter(lrd -> lrd.dim),
                    Codec.unboundedMap(UUIDUtil.STRING_CODEC, MarkedRegion.CODEC).optionalFieldOf("local_regions", Lifecycle.stable(), new HashMap<>(), Lifecycle.stable())
                            .forGetter(lrd -> lrd.locals)
            ).apply(instance, LevelData::new));

    public static SavedDataType<LevelData> buildSavedDataType(Identifier dimRl) {
        var dimId = dimRl.toString().replace(Identifier.NAMESPACE_SEPARATOR, '_');
        var levelId = Identifier.fromNamespaceAndPath(Constants.MOD_ID, dimId);
        return new SavedDataType<>(levelId, () -> new LevelData(dimRl), CODEC,null);
    }

    private final Identifier id;
    private final HashMap<String, UUID> nameIndex;
    private final HashMap<UUID, IMarkableRegion> locals;
    private DimensionalRegion dim;

    public LevelData(Identifier id, DimensionalRegion dim, Map<UUID, IMarkableRegion> locals) {
        this(id);
        this.dim = dim;
        this.locals.putAll(locals);
        locals.forEach( ((uuid, region) -> nameIndex.put(region.getName(), uuid)) );
    }

    public LevelData(Identifier id) {
        this.id = id;
        this.locals = new HashMap<>();
        this.nameIndex = new HashMap<>();
        ResourceKey<Level> levelRk = ResourceKey.create(Registries.DIMENSION, id);
        // TODO restore hierarchy - this will be the case after
        // 1. creation
        // 2. loading
        // 3. tracking new dim
        var dimUuid = UUID.nameUUIDFromBytes(levelRk.identifier().toString().getBytes());
        this.dim = new DimensionalRegion(levelRk, dimUuid, GlobalRegion.GLOBAL_REGION_UUID);
    }

    public static UUID levelUuid(Identifier identifier) {
        return UUID.nameUUIDFromBytes(identifier.toString().getBytes());
    }

    public HashMap<UUID, IMarkableRegion> getLocals() {
        return locals;
    }

    public Identifier getId() {
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

    public Set<String> getLocalNames() {
        return nameIndex.keySet();
    }

    public Set<String> getLocalIds() {
        return locals.values().stream()
                .map(region -> region.getId().toString())
                .collect(Collectors.toSet());
    }

    public DimensionalRegion getDim() {
        return dim;
    }

    public void renameLocal(IMarkableRegion region, String newName) {
        if (this.nameIndex.containsKey(newName)) {
            throw new IllegalArgumentException("Region with name '" + newName + "' already exists in dimension '" + this.dim.getName() + "'!");
        }
        String oldName = region.getName();
        nameIndex.remove(oldName);
        nameIndex.put(newName, region.getUuid());
        region.rename(newName);
    }

    /**
     * Method to check if a region name is valid for a given dimension. <br>
     * A region name is valid if it matches the pattern and is not already used in the dimension.
     *
     * @param regionName the name of the region to be checked.
     * @return -1 if the region name is invalid, 0 if the region name is valid, 1 if the region name is already used in the dimension.
     */
    public static boolean isValidRegionName(String regionName) {
        var valid = Identifier.isValidPath(regionName);
        var validAndMinLength = valid && regionName.length() > 2;
        return validAndMinLength && !CommandConstants.isCommandStr(regionName);
    }

    public void addLocal(IMarkableRegion region) {
        locals.put(region.getUuid(), region);
        nameIndex.put(region.getName(), region.getUuid());
    }

    public void removeLocal(IMarkableRegion region) {
        locals.remove(region.getUuid());
        nameIndex.remove(region.getName());
    }

    public boolean hasLocal(UUID regionId) {
        return locals.containsKey(regionId);
    }

    public boolean hasLocal(String name) {
        UUID id = nameIndex.get(name);
        if (id == null)
            return false;
        return locals.containsKey(id);
    }

    @Nullable
    public IMarkableRegion getLocal(String regionName) {
        UUID id = nameIndex.get(regionName);
        if (id == null) return null;
        return locals.get(id);
    }

    @Nullable
    public IMarkableRegion getLocal(UUID regionId) {
        return locals.get(regionId);
    }

    public void clearLocals() {
        this.locals.clear();
        this.nameIndex.clear();
        this.dim.clearChildren();
    }


}

