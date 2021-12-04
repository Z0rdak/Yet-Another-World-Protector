package de.z0rdak.regionshield.core.region;

import de.z0rdak.regionshield.core.flag.Flag;
import de.z0rdak.regionshield.core.flag.IFlag;
import de.z0rdak.regionshield.core.flag.RegionFlag;
import de.z0rdak.regionshield.util.PlayerUtils;
import de.z0rdak.regionshield.util.constants.RegionNBT;
import net.minecraft.entity.player.PlayerEntity;
import net.minecraft.nbt.CompoundNBT;
import net.minecraft.nbt.ListNBT;
import net.minecraftforge.common.util.Constants;

import java.beans.PropertyChangeSupport;
import java.util.*;

/**
 * A abstract region represents the basic implementation of a IProtectedRegion.
 * This abstraction can be used for markable regions as well as regions without
 * an area (dimensions).
 */
public abstract class AbstractRegion implements IProtectedRegion {

    protected Map<String, IFlag> regionFlags;
    protected Map<UUID, String> members;
    protected boolean isActive;
    protected PropertyChangeSupport propertyChange;

    // TODO: (command) define region with members/owners
    public AbstractRegion() {
        this.propertyChange = new PropertyChangeSupport(this);
        this.regionFlags = new HashMap<>(0);
        this.members = new HashMap<>(0);
        this.isActive = true;
    }

    @Override
    public boolean addFlag(IFlag flag){
        return this.regionFlags.put(flag.getFlagName(), flag) == null;
    }

    public boolean addFlag(String flag, boolean allow) {
        // TODO:
        return this.regionFlags.put(flag, new Flag(flag, allow)) == null;
    }

    public boolean containsFlag(String flag) {
        return this.regionFlags.containsKey(flag);
    }

    // make sure to check contains before
    public boolean denies(String flag){
        return this.regionFlags.get(flag).isDefaultValue();
    }

    public boolean denies(RegionFlag flag){
        return this.denies(flag.toString());
    }

    @Override
    public boolean removeFlag(String flag) {
        return this.regionFlags.remove(flag) != null;
    }

    public boolean containsFlag(RegionFlag flag) {
        return this.containsFlag(flag.toString());
    }

    @Override
    public Set<String> getFlags() {
        return Collections.unmodifiableSet(this.regionFlags.keySet());
    }

    @Override
    public Collection<IFlag> getRegionFlags() {
        return Collections.unmodifiableList(new ArrayList<>(this.regionFlags.values()));
    }

    @Override
    public boolean addPlayer(PlayerEntity player) {
        if (this.members.containsKey(player.getUUID())) {
            return false;
        } else {
            this.members.put(player.getUUID(), player.getDisplayName().getString());
            return true;
        }
    }

    @Override
    public boolean addPlayer(PlayerUtils.MCPlayerInfo playerInfo) {
        String oldPlayer = this.members.put(UUID.fromString(playerInfo.playerUUID), playerInfo.playerName);
        return !playerInfo.playerName.equals(oldPlayer);
    }

    @Override
    public boolean removePlayer(String playerName) {
        Optional<UUID> playerUUID = this.members.entrySet().stream()
                .filter((entry) -> entry.getValue().equals(playerName))
                .findFirst().map(Map.Entry::getKey);
        if (playerUUID.isPresent()) {
            String oldPlayer = this.members.remove(playerUUID.get());
            return oldPlayer != null;
        }
        return false;
    }

    @Override
    public boolean removePlayer(PlayerEntity player) {
        return this.members.remove(player.getUUID()) != null;
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
    public Map<UUID, String> getMembers() {
        return Collections.unmodifiableMap(this.members);
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
    public boolean permits(PlayerEntity player) {
        if (PlayerUtils.hasNeededOpLevel(player)) {
            return true;
        }
        return members.containsKey(player.getUUID());
    }

    @Override
    public boolean forbids(PlayerEntity player) {
        return !this.permits(player);
    }

    @Override
    public CompoundNBT serializeNBT() {
        CompoundNBT nbt = new CompoundNBT();
        nbt.putBoolean(RegionNBT.ACTIVE, isActive);

        // flag data
        ListNBT flagListNBT = new ListNBT();
        this.regionFlags.forEach((flagName, flag) -> {
            flagListNBT.add(flag.serializeNBT());
        });
        nbt.put(RegionNBT.FLAGS, flagListNBT);

        // serialize player data
        ListNBT l = new ListNBT();
        ListNBT playerList = new ListNBT();
        members.forEach((uuid, name) -> {
            CompoundNBT playerNBT = new CompoundNBT();
            playerNBT.putUUID(RegionNBT.UUID, uuid);
            playerNBT.putString(RegionNBT.NAME, name);
            playerList.add(playerNBT);
        });
        nbt.put(RegionNBT.PLAYERS, playerList);
        return nbt;
    }

    @Override
    public void deserializeNBT(CompoundNBT nbt) {
        this.isActive = nbt.getBoolean(RegionNBT.ACTIVE);

        // deserialize flag data
        this.regionFlags.clear();
        ListNBT flagsList = nbt.getList(RegionNBT.FLAGS, Constants.NBT.TAG_COMPOUND);
        for (int i = 0; i < flagsList.size(); i++) {
            Flag flag = new Flag(flagsList.getCompound(i));
            this.regionFlags.put(flag.getFlagName(), flag);
        }

        // deserialize player data
        this.members.clear();
        ListNBT playerLists = nbt.getList(RegionNBT.PLAYERS, Constants.NBT.TAG_COMPOUND);
        for (int i = 0; i < playerLists.size(); i++) {
            CompoundNBT playerMapping = playerLists.getCompound(i);
            members.put(playerMapping.getUUID(RegionNBT.UUID),
                    playerMapping.getString(RegionNBT.NAME));
        }
    }
}
