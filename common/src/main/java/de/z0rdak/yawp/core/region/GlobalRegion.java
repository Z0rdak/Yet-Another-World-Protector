package de.z0rdak.yawp.core.region;

import com.mojang.serialization.Codec;
import com.mojang.serialization.codecs.RecordCodecBuilder;
import de.z0rdak.yawp.core.flag.FlagValue;
import de.z0rdak.yawp.core.flag.RegionFlags;
import de.z0rdak.yawp.core.group.PlayerContainer;
import net.minecraft.core.registries.Registries;
import net.minecraft.resources.ResourceKey;
import net.minecraft.resources.Identifier;
import net.minecraft.world.level.Level;

import java.util.*;
import java.util.stream.Collectors;

public class GlobalRegion extends ProtectedRegion {
    public static final Codec<GlobalRegion> CODEC = RecordCodecBuilder.create(
            instance -> instance.group(
                            Codec.STRING.fieldOf("name")
                                    .forGetter(ProtectedRegion::getName),
                            ResourceKey.codec(Registries.DIMENSION)
                                    .fieldOf("dimension")
                                    .forGetter(ProtectedRegion::getDim),
                            Codec.STRING.fieldOf("parentName")
                                    .forGetter(ProtectedRegion::getParentName),
                            Codec.STRING.fieldOf("type")
                                    .forGetter(r -> r.getRegionType().type),
                            Codec.unboundedMap(Codec.STRING, FlagValue.CODEC)
                                    .fieldOf("flags")
                                    .forGetter(r -> r.getFlags().getFlagMap()),
                            Codec.BOOL.fieldOf("isActive")
                                    .forGetter(ProtectedRegion::isActive),
                            Codec.BOOL.fieldOf("isMuted")
                                    .forGetter(ProtectedRegion::isMuted),
                            Codec.unboundedMap(Codec.STRING, PlayerContainer.CODEC).fieldOf("groups")
                                    .forGetter(ProtectedRegion::getGroups),
                            Codec.list(Codec.STRING).fieldOf("childrenNames")
                                    .forGetter(r -> new ArrayList<>(r.getChildrenNames()))
                    )
                    .apply(instance, (name, dim, parentName, regionType,
                                      flags, isActive, isMuted, groups, childrenNames) ->
                            new GlobalRegion(new RegionFlags(flags), isActive, isMuted, groups, childrenNames)
                    )
    );



    public static final Identifier GLOBAL = Identifier.fromNamespaceAndPath("yawp", "global");
    public static final ResourceKey<Level> GLOBAL_DIMENSION = ResourceKey.create(Registries.DIMENSION, GLOBAL);

    public GlobalRegion() {
        this(GLOBAL.toString(), RegionType.GLOBAL);
        this.setParent(this);
    }

    private GlobalRegion(RegionFlags flags, boolean isActive, boolean isMuted, Map<String, PlayerContainer> groups, List<String> childrenNames) {
        this(GLOBAL.toString(), RegionType.GLOBAL);
        this.setParent(this);
        this.setFlags(flags);
        this.setIsActive(isActive);
        this.setIsMuted(isMuted);
        this.setGroups(groups);
        this.setChildrenNames(childrenNames);
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
}
