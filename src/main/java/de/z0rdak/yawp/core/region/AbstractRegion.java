package de.z0rdak.yawp.core.region;

import de.z0rdak.yawp.core.affiliation.PlayerContainer;
import de.z0rdak.yawp.core.flag.FlagContainer;
import de.z0rdak.yawp.core.flag.IFlag;
import de.z0rdak.yawp.core.flag.RegionFlag;
import net.minecraft.nbt.CompoundTag;
import net.minecraft.world.entity.player.Player;
import net.minecraft.world.scores.Team;

import java.util.*;

import static de.z0rdak.yawp.util.constants.RegionNBT.*;

/**
 * A abstract region represents the basic implementation of a IProtectedRegion.
 * This abstraction can be used for markable regions as well as regions without
 * an area (dimensions).
 */
public abstract class AbstractRegion implements IProtectedRegion {
    protected String name;
    protected RegionType regionType;
    protected FlagContainer flags;
    protected PlayerContainer owners;
    protected PlayerContainer members;
    protected boolean isActive;

    protected AbstractRegion(CompoundTag nbt) {
        this.deserializeNBT(nbt);
    }

    protected AbstractRegion(String name, RegionType type) {
        this.name = name;
        this.regionType = type;
        this.flags = new FlagContainer();
        this.members = new PlayerContainer();
        this.owners = new PlayerContainer();
        this.isActive = true;
    }

    /**
     * Minimal constructor to create a abstract region by supplying a name, type and an owner.
     * @param name name of the region
     * @param owner region owner
     */
    protected AbstractRegion(String name, RegionType regionType, Player owner) {
        this(name, regionType);
        if (owner != null) {
            this.owners.addPlayer(owner);
        }
    }

    @Override
    public String getName() {
        return name;
    }

    @Override
    public void addFlag(IFlag flag){
        this.flags.put(flag);
    }

    public boolean containsFlag(IFlag flag) {
        return this.flags.contains(flag);
}

    @Override
    public void removeFlag(String flag) {
        this.flags.remove(flag);
    }

    public boolean containsFlag(RegionFlag flag) {
        return this.flags.contains(flag.flag);
    }

    @Override
    public Collection<IFlag> getFlags() {
        return List.copyOf(this.flags.values());
    }

    @Override
    public boolean isActive() {
        return this.isActive;
    }

    @Override
    public void setIsActive(boolean isActive) {
        this.isActive = isActive;
    }

    @Override
    public void addMember(Player player) {
        this.members.addPlayer(player);
    }

    @Override
    public void addMember(Team team) {
        this.members.addTeam(team);
    }


    @Override
    public void addOwner(Player player) {
        this.owners.addPlayer(player);
    }

    @Override
    public void addOwner(Team team) {
        this.owners.addTeam(team);
    }

    @Override
    public void removeMember(Player player) {
        this.members.removePlayer(player);

    }

    @Override
    public void removeMember(Team team) {
        this.members.removeTeam(team.getName());

    }

    @Override
    public void removeOwner(Player player) {
        this.owners.removePlayer(player);
    }

    @Override
    public void removeOwner(Team team) {
        this.owners.removeTeam(team);

    }

    @Override
    public PlayerContainer getMembers() {
        return this.members;
    }

    @Override
    public PlayerContainer getOwners() {
        return owners;
    }

    /**
     * Checks if the player is defined in the regions player list OR whether the player is an operator.
     * Usually this check is needed when an event occurs and it needs to be checked whether
     * the player has a specific permission to perform an action in the region.
     *
     * @param player to be checked
     * @return true if player is in region list or is an operator, false otherwise
     */
    @Override
    public boolean permits(Player player) {
        boolean isOwner = this.owners.containsPlayer(player.getUUID())
                || (player.getTeam() != null && this.owners.containsTeam(player.getTeam().getName()));
        boolean isMember = this.members.containsPlayer(player.getUUID())
                || (player.getTeam() != null && this.members.containsTeam(player.getTeam().getName()));
        return isOwner || isMember;
    }

    @Override
    public CompoundTag serializeNBT() {
        CompoundTag nbt = new CompoundTag();
        nbt.putString(NAME, this.name);
        nbt.putBoolean(ACTIVE, this.isActive);
        nbt.putString(REGION_TYPE, this.regionType.type);
        nbt.put(FLAGS, this.flags.serializeNBT());
        nbt.put(OWNERS, this.owners.serializeNBT());
        nbt.put(MEMBERS, this.members.serializeNBT());
        return nbt;
    }

    @Override
    public void deserializeNBT(CompoundTag nbt) {
        this.name = nbt.getString(NAME);
        this.isActive = nbt.getBoolean(ACTIVE);
        this.regionType = RegionType.of(nbt.getString(REGION_TYPE));
        this.flags = new FlagContainer(nbt.getCompound(FLAGS));
        this.owners = new PlayerContainer(nbt.getCompound(OWNERS));
        this.members = new PlayerContainer(nbt.getCompound(MEMBERS));
    }
}
