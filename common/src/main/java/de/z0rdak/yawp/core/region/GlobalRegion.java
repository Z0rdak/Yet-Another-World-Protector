package de.z0rdak.yawp.core.region;

import net.minecraft.core.registries.Registries;
import net.minecraft.nbt.CompoundTag;
import net.minecraft.resources.ResourceKey;
import net.minecraft.resources.ResourceLocation;
import net.minecraft.world.level.Level;

import java.util.Collections;
import java.util.Map;
import java.util.Set;
import java.util.stream.Collectors;

public class GlobalRegion extends AbstractRegion {

    public static final ResourceLocation GLOBAL = ResourceLocation.fromNamespaceAndPath("yawp", "global");
    public static final ResourceKey<Level> GLOBAL_DIMENSION = ResourceKey.create(Registries.DIMENSION, GLOBAL);

    public GlobalRegion(CompoundTag nbt) {
        super(nbt);
    }

    public GlobalRegion() {
        this(GLOBAL.toString(), RegionType.GLOBAL);
        this.setParent(this);
    }

    protected GlobalRegion(String name, RegionType type) {
        super(name, GLOBAL_DIMENSION, type);
        super.setParent(this);
    }

    @Override
    public Map<String, IProtectedRegion> getChildren() {
        Map<String, IProtectedRegion> childrenWithoutGlobal = super.getChildren().entrySet().stream()
                .filter(e -> e.getValue().getRegionType() != RegionType.GLOBAL)
                .collect(Collectors.toMap(Map.Entry::getKey, Map.Entry::getValue));
        return Collections.unmodifiableMap(childrenWithoutGlobal);
    }

    @Override
    public Set<String> getChildrenNames() {
        Set<String> childrenWithoutGlobal = super.getChildren().values().stream()
                .filter(iProtectedRegion -> iProtectedRegion.getRegionType() != RegionType.GLOBAL)
                .map(IProtectedRegion::getName)
                .collect(Collectors.toSet());
        return Collections.unmodifiableSet(childrenWithoutGlobal);
    }

    @Override
    protected boolean setParent(IProtectedRegion parent) {
        if (parent.getRegionType() == RegionType.GLOBAL) {
            return super.setParent(parent);
        }
        return false;
    }

    @Override
    public boolean addChild(IProtectedRegion child) {
        if (child.getRegionType() == RegionType.DIMENSION) {
            return super.addChild(child);
        }
        return false;
    }

    @Override
    public CompoundTag serializeNBT() {
        CompoundTag nbt = super.serializeNBT();
        return nbt;
    }

    @Override
    public void deserializeNBT(CompoundTag nbt) {
        super.deserializeNBT(nbt);
    }
}
